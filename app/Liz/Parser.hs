{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Parser where

import qualified Liz.Common.Errors as E
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Liz.Common.Types

import Data.String (IsString (..))
import Data.Char (isAlphaNum, isDigit, isPrint) 
import Control.Monad (void, liftM)
import Data.List (find)

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

invalidNumber :: T.Text -> Parser a
invalidNumber = customFailure . E.InvalidNumber

tooManyExprsIf :: Parser a
tooManyExprsIf = customFailure E.TooManyExprsIf

wrongArgCount :: Int -> Int -> Parser a
wrongArgCount ex got = customFailure $ E.WrongArgCount ex got

-- helper parsing functions
getCurrentPos :: Parser (Pos, Pos) 
getCurrentPos = getSourcePos >>= \p -> pure (sourceLine p, sourceColumn p)

head' :: [a] -> a
head' = NE.head . NE.fromList

last' :: [a] -> a
last' = NE.last . NE.fromList

scn :: Parser ()
scn = L.space space1 (void $ spaceChar <|> tab) empty

lizReserved :: [T.Text]
lizReserved = 
  [ "var", "set", "const", "if", "def", "return", "False",
    "True", "not", "negate", "Int", "Float", 
    "String", "Char", "Bool", "Unit", "print", "block",
    "macro"]

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

parseValue :: Parser Expression
parseValue = do
  s <- getCurrentPos
  v <- choice [
      ELiteral String' <$> parseStr, 
      ELiteral Char' <$> parseChar,
      parseNum, 
      ELiteral Bool' <$> parseBool,
      ELiteral Unit' <$> parseUnit,
      EIdentifier <$> parseIdent False
    ]
  e <- getCurrentPos
  pure $ v s e

parseNested :: Parser SExpr
parseNested = do
  v <- (SEExpr <$> (parseValueMacro <|> parseValue)) <|> parseSExpr
  pure v

parseExpr :: Parser Expression
parseExpr = parseValue <|> parseValueMacro <|> (between (char '(') (char ')') $ 
  choice [
    parseVarDecl,
    parseSetStmt,
    parseRet,
    parsePrint,
    parseFuncCall
  ]) 

-- main parsing functions
parseIdent :: Bool -> Parser T.Text
parseIdent shouldParseOp = do
  s <- letterChar
  r <- takeWhileP (Just "alphanumeric character or '_' or '-'.") valid
  let ident = T.pack [s] <> r

  if ident `elem` lizReserved && (not shouldParseOp)
  then reservedIdent ident
  else pure $ ident
  where
    valid :: Char -> Bool 
    valid c = (liftA2 (||) isAlphaNum ('_' ==) c) || ('-' == c)

parseUnit :: Parser T.Text
parseUnit = string "()"

parseStr :: Parser T.Text
parseStr = do
  _ <- char '"'
  str <- takeWhile1P (Just "alphanumeric character.") valid
  _ <- char '"'
  pure str
  where
    valid :: Char -> Bool
    valid = liftA2 (&&) isPrint ('"' /=)

parseChar :: Parser T.Text
parseChar = do
  _ <- char '\''
  c <- printChar
  _ <- char '\''
  pure $ T.pack [c]

parseNum :: Parser (LizPos -> LizPos -> Expression)
parseNum = do
  n <- (takeWhile1P @_ @T.Text (Just "digits 0-9 or '.'") valid) <* notFollowedBy letterChar
  case () of _
              | count '.' n == 1 -> pure $ ELiteral Float' n
              | count '.' n == 0 -> pure $ ELiteral Int' n
              | otherwise -> invalidNumber n
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isDigit ('.' ==) 

    count :: Char -> T.Text -> Int
    count c = T.length . T.filter (c ==)

parseBool :: Parser T.Text
parseBool = string "True" <|> string "False"

parseValueMacro :: Parser Expression
parseValueMacro = do
  s <- getCurrentPos
  _ <- char '%'
  i <- parseIdent False
  e <- getCurrentPos
  pure $ EValueMacro i s e

parsePrint :: Parser Expression
parsePrint = do
  s <- getCurrentPos
  _ <- string "print"
  hspace1
  v <- parseNested
  e <- getCurrentPos
  pure $ EPrint s e v

parseRet :: Parser Expression
parseRet = do
  s <- getCurrentPos
  _ <- string "return"
  hspace1
  v <- parseNested
  e <- getCurrentPos
  pure $ EReturn s e v

parseMacroDef :: Parser SExpr
parseMacroDef = do
  s <- getCurrentPos
  _ <- string "macro"
  hspace1
  i <- parseIdent False
  hspace1
  v <- parseNested
  e <- getCurrentPos
  pure $ SEMacroDef $ Macro s e i v

parseComment :: Parser SExpr
parseComment = do
  _ <- char ';'
  _ <- hidden $ takeWhileP Nothing valid
  pure SEComment
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isPrint ('\n' /=)

parseVarDecl :: Parser Expression
parseVarDecl = do
  s <- getCurrentPos
  decType <- EVar s <$ string "var" <|> EConst s <$ string "const"
  hspace1
  ident <- parseIdent False
  hspace1 
  ty <- (liftM SEType parseType) <|> parseNested
  aux decType ident ty
  where
    aux decl iden (SEType ty) = do
      _ <- some hspace1
      value <- L.lineFold scn $ \_ -> parseNested
      e <- getCurrentPos
      pure $ decl e Var{varIdent=iden, varType=ty, varValue=value}
    aux decl iden lit@(SEExpr (ELiteral ty _ _ _)) = do
      e <- getCurrentPos
      pure $ decl e Var{varIdent=iden, varType=ty, varValue=lit}
    aux _ _ op = unsupportedDeclaration $ T.show op

parseSetStmt :: Parser Expression
parseSetStmt = do
  s <- getCurrentPos
  _ <- string "set"
  hspace1
  ident <- parseIdent False
  hspace1
  value <- parseNested
  e <- getCurrentPos
  pure $ ESet s e ident value

parseFuncDecl :: Parser SExpr
parseFuncDecl = do
   s <- getCurrentPos
   _ <- string "def"
   hspace1
   ident <- (parseIdent False) <|> parseOpId
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
         ident <- (parseIdent False)
         hidden hspace
         _ <- char '~'
         hidden hspace
         ty <- parseType
         pure Arg {argIdent = ident, argType = ty}

parseFuncCall :: Parser Expression
parseFuncCall = do
  s <- getCurrentPos
  ident <- (parseIdent True) <|> parseOpId
  hspace1
  args <- parseCallArgs
  e <- getCurrentPos
  let
    binaryRes = find ((==) ident . fst) binaryOps
    unaryRes = find ((==) ident . fst) unaryOps
  case binaryRes of
    Just op -> 
        if length args == 2 
        then 
          let 
            l = head' args 
            r = last' args
          in pure $ EBinary (snd op) s e l r
        else wrongArgCount 2 (length args)
    Nothing ->
      case unaryRes of
        Just op ->
          if length args == 1 
          then pure $ EUnary (snd op) s e (head' args) 
          else wrongArgCount 1 (length args)
        Nothing -> pure $ EFuncCall s e ident args
  where
    parseCallArgs :: Parser [SExpr]
    parseCallArgs = parseNested `sepBy` char ' '

    parseOpId :: Parser T.Text
    parseOpId = liftM T.pack $ some lizSymbols

    binaryOps :: [(T.Text, BinaryOp)]
    binaryOps = [
        ("++", Concat),
        ("==", Eql),
        ("!=", NotEql),
        ("+", Add),
        ("-", Subtract),
        ("*", Multiply),
        ("/", Divide),
        (">=", GreaterEql),
        ("<=", LessEql),
        (">", Greater),
        ("<", Less)
      ]

    unaryOps :: [(T.Text, UnaryOp)]
    unaryOps = [
        ("not", Not), 
        ("negate", Negate)
      ]

parseBlock :: Parser SExpr
parseBlock = do
  s <- getCurrentPos
  _ <- string "block"
  hspace
  block <- some $ L.lineFold scn $ \_ -> parseSExpr
  e <- getCurrentPos
  pure $ SEBlockStmt s e block

parseIfStmt :: Parser SExpr
parseIfStmt = do
  s <- getCurrentPos
  _ <- string "if"
  hspace
  cond <- parseExpr
  hspace
  block <- some $ L.lineFold scn $ \_ -> parseNested <?> "if-sexpr branch"
  e <- getCurrentPos
  case () of _
              | length block > 2 -> tooManyExprsIf
              | length block == 1 -> pure $ SEIfStmt s e cond (head' block) Nothing
              | otherwise ->
                let 
                  truebr = head' block 
                  falsebr = last' block
                in
                pure $ SEIfStmt s e cond truebr (Just falsebr)

parseSExpr :: Parser SExpr
parseSExpr = (between (char '(') (char ')') $ 
  label "valid S-Expression" 
    (choice [parseFuncDecl
            , SEExpr <$> parseVarDecl
            , SEExpr <$> parseSetStmt
            , parseIfStmt
            , SEExpr <$> parseRet
            , SEExpr <$> parsePrint
            , parseMacroDef
            , parseBlock
            , SEExpr <$> parseFuncCall
            ])) <|> parseComment

parseProgram :: Parser [SExpr]
parseProgram = some $ scn >> parseSExpr

parseFile :: FilePath -> T.Text -> Either (ParseErrorBundle T.Text E.PError) Program
parseFile f fc = do
  case (parse parseProgram f fc) of
    (Left err) -> Left err
    (Right v) -> Right $ Program (removeComments v)
  where
    removeComments :: [SExpr] -> [SExpr]
    removeComments [] = []
    removeComments (SEComment : rest) = removeComments rest
    removeComments (SEFunc func@(Func{funcBody=body}) : rest) = SEFunc func{funcBody=filter (SEComment /=) body} : removeComments rest
    removeComments (SEBlockStmt s e body : rest) = SEBlockStmt s e (filter (SEComment /=) body) : removeComments rest
    removeComments (expr : rest) = expr : removeComments rest
