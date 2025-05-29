{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Parser where

import qualified Liz.Common.Error as E
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

-- helper parsing functions
getCurrentPos :: Parser (Pos, Pos) 
getCurrentPos = getSourcePos >>= \p -> pure (sourceLine p, sourceColumn p)

lizReserved :: [T.Text]
lizReserved = 
  [ "var", "set", "const", "if", "func", "return", "False",
    "True", "undefined", "not", "negate", "Int", "Float", 
    "String", "Char", "Bool", "Unit", "print" ]

parseType :: Parser Type
parseType =
  Int'    <$ string "Int" <|>
  Float'  <$ string "Float" <|>
  String' <$ string "String" <|>
  Char'   <$ string "Char" <|>
  Bool'   <$ string "Bool" <|>
  Unit'   <$ string "Unit"

parseValue :: Parser T.Text
parseValue = choice [parseStr, parseChar, parseNum, parseBool, parseUndefined, parseUnit]

parseNested :: Parser SExpr
parseNested = do
  s <- getCurrentPos
  v <- (do
    y <- (SELiteral <$> parseValue) <|> (SEIdentifier <$> parseIdent)
    e <- getCurrentPos
    pure $ y s e) <|> parseSExpr
  pure v

-- main parsing functions
parseIdent :: Parser T.Text
parseIdent = do
  s <- letterChar
  r <- takeWhileP (Just "alphanumeric character or '_'") valid
  let ident = T.pack [s] <> r

  if ident `elem` lizReserved
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

parseNum :: Parser T.Text
parseNum = (takeWhile1P (Just "digits 0-9 or '.'") valid) <* notFollowedBy letterChar
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isDigit ('.' ==) 

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
  ident <- parseIdent
  hspace1 
  ty <- (liftM SEType parseType) <|> parseNested
  aux decType ident ty
  where
    aux decl iden (SEType ty) = do
      hspace1
      value <- parseNested
      e <- getCurrentPos
      pure $ decl e Var{varIdent=iden, varType=ty, varValue=value}
    aux decl iden lit@(SELiteral literal _ _) = do
      ty <- inferType literal
      e <- getCurrentPos
      pure $ decl e Var{varIdent=iden, varType=ty, varValue=lit}
    aux _ _ op = unsupportedDeclaration $ T.show op

    inferType :: T.Text -> Parser Type
    inferType v
      | (count '.' v) == 1 =
        if (==) 0 $ (removeDigits . T.filter ((/=) '.')) v
        then pure Float'
        else failedTypeInference v
      | removeDigits v == 0 = pure  Int'
      | (T.take 1 v) == "'" && (T.last v) == '\'' = pure Char'
      | (T.take 1 v) == "\"" && (T.last v) == '"' = pure String'
      | v == "True" || v == "False" = pure Bool'
      | v == "()" = pure Unit'
      | v == "undefined" = inferredUndefined
      | otherwise = failedTypeInference v
      where
        removeDigits :: T.Text -> Int
        removeDigits = T.length . T.filter (not . isDigit)

        count :: Char -> T.Text -> Int
        count t = (T.length . T.filter (t ==))

parseSetStmt :: Parser SExpr
parseSetStmt = do
  s <- getCurrentPos
  _ <- string "set"
  hspace1
  ident <- parseIdent
  hspace1
  value <- parseNested
  e <- getCurrentPos
  pure $ SESet s e ident value

parseUnary :: Parser SExpr
parseUnary = do
  s <- getCurrentPos
  op <- parseUnaryOp
  hspace1
  v <- parseNested
  e <- getCurrentPos
  pure $ op s e v
  where
  parseUnaryOp = SEUnary Not <$ string "not" <|> SEUnary Negate <$ string "negate"

parseBinary :: Parser SExpr
parseBinary = do
  s <- getCurrentPos
  op <- parseBinaryOp
  hspace1
  left <- parseNested
  hspace1
  right <- parseNested
  e <- getCurrentPos
  pure $ op s e left right
  where
    parseBinaryOp = 
      SEBinary Concat <$ string "++" <|>
      SEBinary Add <$ string "+" <|> 
      SEBinary Subtract <$ string "-" <|> 
      SEBinary Multiply <$ string "*" <|> 
      SEBinary Divide <$ string "/" <|> 
      SEBinary GreaterEql <$ string ">=" <|> 
      SEBinary LessEql <$ string "<=" <|> 
      SEBinary Eql <$ string "==" <|> 
      SEBinary NotEql <$ string "!=" <|>
      SEBinary Greater <$ string ">" <|> 
      SEBinary Less <$ string "<"

{-
  (func *ident* *args* *return type* *body*)
    (func flip [b ~ Bool] > Bool 
     (not b))

    (func say_hi [name ~ String] > Unit
     (const with_hello String (++ "hello " name))
     (print with_hello)
     (return ())
-}
parseFuncDecl :: Parser SExpr
parseFuncDecl = do
   s <- getCurrentPos
   _ <- string "func"
   hspace1
   ident <- parseIdent
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

     parseFuncArgs :: Parser [Arg]
     parseFuncArgs = between (char '[') (char ']') $ aux `sepBy` char ','
       where
         aux :: Parser Arg
         aux = do
           hidden hspace
           ident <- parseIdent
           hidden hspace
           _ <- char '~'
           hidden hspace
           ty <- parseType
           pure Arg {argIdent = ident, argType = ty}

parseFuncCall :: Parser SExpr
parseFuncCall = do
  s <- getCurrentPos
  ident <- parseIdent
  hspace1
  args <- parseCallArgs
  e <- getCurrentPos
  pure $ SEFuncCall s e ident args
  where
    parseCallArgs :: Parser [SExpr]
    parseCallArgs = parseNested `sepBy` char ' '

parseSExpr :: Parser SExpr
parseSExpr = (between (char '(') (char ')') $ 
  label "valid S-Expression" 
    (choice [parseFuncDecl
            , parseBinary
            , parseUnary
            , parseVarDecl
            , parseSetStmt
            , parseRet
            , parsePrint
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

parseAndPP :: FilePath -> IO ()
parseAndPP f = do
  fc <- liftM T.pack $ readFile f
  case (parse parseProgram f fc) of
    (Left err) -> putStrLn $ errorBundlePretty err
    (Right v) -> putStrLn $ foldMap show v
