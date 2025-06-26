{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Parser where

import qualified Liz.Common.Errors as E
import qualified Data.Text as T
import Liz.Common.Types

import Data.String ( IsString (..))
import Data.Char (isAlphaNum, isDigit, isPrint) 
import Control.Monad (void, liftM)

import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

default IsString (T.Text)
type Parser = Parsec E.PError T.Text

-- helper error functions
failedTypeInference :: T.Text -> Parser a
failedTypeInference = customFailure . E.FailedTypeInference

reservedIdent :: T.Text -> Parser a
reservedIdent = customFailure . E.ReservedIdent

unsupportedDeclaration :: T.Text -> Parser a
unsupportedDeclaration = customFailure . E.UnsupportedDeclaration

inferredUndefined :: Parser a
inferredUndefined = customFailure E.InferredUndefined

invalidNumber :: T.Text -> Parser a
invalidNumber = customFailure . E.InvalidNumber

tooManyExprsIf :: Parser a
tooManyExprsIf = customFailure E.TooManyExprsIf

wrongArgCount :: Int -> Int -> Parser a
wrongArgCount ex got = customFailure $ E.WrongArgCount ex got

-- helper parsing functions
getCurrentPos :: Parser (Pos, Pos) 
getCurrentPos = getSourcePos >>= \p -> pure (sourceLine p, sourceColumn p)

lizReserved :: [T.Text]
lizReserved = 
  [ "var", "set", "const", "if", "def", "return", "False",
    "True", "undefined", "not", "negate", "Int", "Float", 
    "String", "Char", "Bool", "Unit", "print", "block"]

lizSymbols :: Parser Char
lizSymbols = choice
  [char '*', char '/', char '-', char '=', char '+', char '<',
   char '>', char '?', char '.', char '^', char '%', char '$',
   char '#', char '£', char '@', char '!', char '\\', char ',',
   char '~']

parseType :: Parser Type
parseType =
  Int'    <$ string "Int" <|>
  Float'  <$ string "Float" <|>
  String' <$ string "String" <|>
  Char'   <$ string "Char" <|>
  Bool'   <$ string "Bool" <|>
  Unit'   <$ string "Unit"

parseValue :: Parser (LizPos -> LizPos -> SExpr)
parseValue = choice [
    SELiteral String' <$> parseStr, 
    SELiteral Char' <$> parseChar,
    parseNum, 
    SELiteral Bool' <$> parseBool,
    SELiteral Undef' <$> parseUndefined,
    SELiteral Unit' <$> parseUnit
  ]

parseNested :: Parser SExpr
parseNested = do
  s <- getCurrentPos
  v <- (do
    value <- parseValue <|> (SEIdentifier <$> parseIdent True)
    e <- getCurrentPos
    pure $ value s e) <|> parseSExpr
  pure v

-- main parsing functions
parseIdent :: Bool -> Parser T.Text
parseIdent flag = do
  s <- letterChar
  r <- takeWhileP (Just "alphanumeric character or '_'") valid
  let ident = T.pack [s] <> r

  if ident `elem` lizReserved && flag
  then reservedIdent ident
  else pure $ ident
  where
    valid :: Char -> Bool 
    valid = liftA2 (||) isAlphaNum ('_' ==)

parseUnit :: Parser T.Text
parseUnit = string "()"

parseUndefined :: Parser T.Text
parseUndefined = string "undefined"

parseStr :: Parser T.Text
parseStr = do
  d1 <- char '"'
  str <- takeWhile1P (Just "alphanumeric character.") valid
  d2 <- char '"'
  pure $ (d1 T.:< str) T.:> d2
  where
    valid :: Char -> Bool
    valid = liftA2 (&&) isPrint ('"' /=)

parseChar :: Parser T.Text
parseChar = do
  d1 <- char '\''
  c <- printChar
  d2 <- char '\''
  pure $ T.pack [d1, c, d2]

parseNum :: Parser (LizPos -> LizPos -> SExpr)
parseNum = do
  n <- (takeWhile1P @_ @T.Text (Just "digits 0-9 or '.'") valid) <* notFollowedBy letterChar
  case () of _
              | count '.' n == 1 -> pure $ SELiteral Float' n
              | count '.' n == 0 -> pure $ SELiteral Int' n
              | otherwise -> invalidNumber n
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isDigit ('.' ==) 

    count :: Char -> T.Text -> Int
    count c = T.length . T.filter (c ==)

parseBool :: Parser T.Text
parseBool = string "True" <|> string "False"

parsePrint :: Parser SExpr
parsePrint = do
  s <- getCurrentPos
  _ <- string "print"
  hspace1
  v <- parseNested
  e <- getCurrentPos
  pure $ SEPrint s e v

parseRet :: Parser SExpr
parseRet = do
  s <- getCurrentPos
  _ <- string "return"
  hspace1
  v <- parseNested
  e <- getCurrentPos
  pure $ SEReturn s e v

parseComment :: Parser SExpr
parseComment = do
  _ <- char ';'
  _ <- hidden $ some $ alphaNumChar <|> punctuationChar <|> char ' '
  pure SEComment

{-
  ({var | const} *ident* *type* *value*)
    (var hello String "World")
    (const four 4) ; inferred Int 
    (var not_allowed (+ 5 6))
                      ^ For now, you can't declare a variable with inferred type using a nested statement.
                        Maybe I'll add support later on.
-}
parseVarDecl :: Parser SExpr
parseVarDecl = do
  s <- getCurrentPos
  decType <- SEVar s <$ string "var" <|> SEConst s <$ string "const"
  hspace1
  ident <- parseIdent True
  hspace1 
  ty <- (liftM SEType parseType) <|> parseNested
  aux decType ident ty
  where
    aux decl iden (SEType ty) = do
      hspace1
      value <- parseNested
      e <- getCurrentPos
      pure $ decl e Var{varIdent=iden, varType=ty, varValue=value}
    aux decl iden lit@(SELiteral ty _ _ _) = do
      e <- getCurrentPos
      pure $ decl e Var{varIdent=iden, varType=ty, varValue=lit}
    aux _ _ op = unsupportedDeclaration $ T.show op

parseSetStmt :: Parser SExpr
parseSetStmt = do
  s <- getCurrentPos
  _ <- string "set"
  hspace1
  ident <- parseIdent True
  hspace1
  value <- parseNested
  e <- getCurrentPos
  pure $ SESet s e ident value

{-
  (def *ident* *args* *return type* *body*)
    (def flip [b ~ Bool] > Bool 
     (not b))

    (def say_hi [name ~ String] > Unit
     (const with_hello String (++ "hello " name))
     (print with_hello)
     (return ())
-}
parseFuncDecl :: Parser SExpr
parseFuncDecl = do
   s <- getCurrentPos
   _ <- string "def"
   hspace1
   ident <- (parseIdent True) <|> parseOpId
   hspace1
   args <- parseFuncArgs
   hidden hspace
   _ <- char '>'
   hidden hspace
   retTy <- parseType

   block <- some $ L.lineFold scn $ \_ -> parseSExpr
   e <- getCurrentPos
   pure $ SEFunc Func {funcIdent = ident, funcStart = s, funcEnd = e, funcArgs = args, funcReturnType = retTy, funcBody = block}
   where
    scn :: Parser ()
    scn = L.space space1 (void $ spaceChar <|> tab) empty

    parseOpId :: Parser T.Text
    parseOpId = do
      _ <- char '('
      op <- liftM T.pack $ some lizSymbols
      _ <- char ')'
      if op `elem` lizReserved then reservedIdent op
                               else pure op

    parseFuncArgs :: Parser [Arg]
    parseFuncArgs = between (char '[') (char ']') $ aux `sepBy` char ','
      where
       aux :: Parser Arg
       aux = do
         hidden hspace
         ident <- (parseIdent True)
         hidden hspace
         _ <- char '~'
         hidden hspace
         ty <- parseType
         pure Arg {argIdent = ident, argType = ty}

-- TODO: sort this out.
-- issue due to binary/unary parsing clashing with custom op parsing.
parseFuncCall :: Parser SExpr
parseFuncCall = do
  s <- getCurrentPos
  ident <- (parseIdent False) <|> parseOpId
  hspace1
  args <- parseCallArgs
  e <- getCurrentPos
  case () of _
  -- TODO: turn the identifier into a binary/unary op
              | ident `elem` binaryOp ->
                if length args == 2 then let [l, r] = args in pure $ SEBinary (fromBinaryOp ident) s e l r
                                    else wrongArgCount 2 (length args)
              | ident `elem` unaryOp ->
                if length args == 1 then let [v] = args in pure $ SEUnary (fromUnaryOp ident) s e v
                                    else wrongArgCount 1 (length args)
              | otherwise -> pure $ SEFuncCall s e ident args
  where
    parseCallArgs :: Parser [SExpr]
    parseCallArgs = parseNested `sepBy` char ' '

    parseOpId :: Parser T.Text
    parseOpId = liftM T.pack $ some lizSymbols

    binaryOp :: [T.Text]
    binaryOp = ["++", "==", "!=", "+", "-", "*", "/", ">=", "<=", ">", "<"]

    unaryOp :: [T.Text]
    unaryOp = ["not", "negate"]

    -- TODO: change the ways these work, I don't like the execeptions.
    fromBinaryOp :: T.Text -> BinaryOp
    fromBinaryOp op =
      case op of
        "++" -> Concat
        "==" -> Eql
        "!=" -> NotEql
        "+" -> Add
        "-" -> Subtract
        "*" -> Multiply
        "/" -> Divide
        ">=" -> GreaterEql
        "<=" -> LessEql
        ">" -> Greater
        "<" -> Less
        _ -> error "Reached unreachable in fromBinaryOp"

    fromUnaryOp :: T.Text -> UnaryOp
    fromUnaryOp op =
      case op of
        "not" -> Not
        "negate" -> Negate
        _ -> error "Reached unreachable in fromUnaryOp"

parseBlock :: Parser SExpr
parseBlock = do
  s <- getCurrentPos
  _ <- string "block"
  hspace
  block <- some $ L.lineFold scn $ \_ -> parseSExpr
  e <- getCurrentPos
  pure $ SEBlockStmt s e block
  where
    scn :: Parser ()
    scn = L.space space1 (void $ spaceChar <|> tab) empty

-- TODO: fix errors here (mainly with empty ifs)
parseIfStmt :: Parser SExpr
parseIfStmt = do
  s <- getCurrentPos
  _ <- string "if"
  hspace
  cond <- parseSExpr
  hspace
  block <- some $ L.lineFold scn $ \_ -> parseSExpr
  e <- getCurrentPos
  case () of _
              | length block > 2 -> tooManyExprsIf
              | length block == 1 -> pure $ SEIfStmt s e cond (head block) Nothing -- using head since the list can't be empty.
              | otherwise ->
                let [truebr, falsebr] = block in
                pure $ SEIfStmt s e cond truebr (Just falsebr)
  where
    scn :: Parser ()
    scn = L.space space1 (void $ spaceChar <|> tab) empty

parseSExpr :: Parser SExpr
parseSExpr = (between (char '(') (char ')') $ 
  label "valid S-Expression" 
    (choice [parseFuncDecl
            , parseVarDecl
            , parseSetStmt
            , parseIfStmt
            , parseRet
            , parsePrint
            , parseBlock
            , parseFuncCall
            ])) <|> parseComment

parseProgram :: Parser [SExpr]
parseProgram = some $ try $ scn >> parseSExpr
  where
    scn :: Parser ()
    scn = L.space space1 empty empty

parseFile :: FilePath -> T.Text -> Either (ParseErrorBundle T.Text E.PError) Program
parseFile f fc = do
  case (parse parseProgram f fc) of
    (Left err) -> Left err
    (Right v) -> Right $ Program v
