{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Parser where

import qualified Liz.Common.Error as E
import qualified Data.Text as T
import Liz.Common.Types

import Data.String ( IsString (..))
import Data.Char (isAlphaNum, isDigit, isPrint)
import Control.Monad (void)

import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

default IsString (T.Text)
type Parser = Parsec E.PError T.Text

newtype Program = Program [SExpr]
  deriving (Show, Eq)

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
    "String", "Char", "Bool", "print" ]

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
  p <- getCurrentPos
  choice [SELiteral p <$> parseValue, SEIdentifier p <$> parseIdent, parseSExpr]

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
  p <- getCurrentPos
  _ <- string "print"
  hspace1
  v <- parseNested
  pure $ SEPrint p v

parseRet :: Parser SExpr
parseRet = do
  p <- getCurrentPos
  _ <- string "return"
  hspace1
  v <- parseNested
  pure $ SEReturn p v

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
  p <- getCurrentPos
  decType <- SEVar p <$ string "var" <|> SEConst p <$ string "const"
  hspace1
  ident <- parseIdent
  hspace1 
  ty <- (parseType >>= pure . SEType) <|> parseNested
  aux decType ident ty
  where
    aux decl iden (SEType ty) = do
      hspace1
      value <- parseNested
      pure $ decl Var{varIdent=iden, varType=ty, varValue=value}
    aux decl iden lit@(SELiteral _ literal) = do
      ty <- inferType literal
      pure $ decl Var{varIdent=iden, varType=ty, varValue=lit}
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
  p <- getCurrentPos
  _ <- string "set"
  hspace1
  ident <- parseIdent
  hspace1
  value <- parseNested
  pure $ SESet p ident value

parseUnary :: Parser SExpr
parseUnary = do
  p <- getCurrentPos
  op <- parseUnaryOp p
  hspace1
  v <- parseNested
  pure $ op v
  where
  parseUnaryOp p = SEUnary p Not <$ string "not" <|> SEUnary p Negate <$ string "negate"

parseBinary :: Parser SExpr
parseBinary = do
  p <- getCurrentPos
  op <- parseBinaryOp p
  hspace1
  left <- parseNested
  hspace1
  right <- parseNested
  pure $ op left right
  where
    parseBinaryOp p = 
      SEBinary p Concat <$ string "++" <|>
      SEBinary p Add <$ string "+" <|> 
      SEBinary p Subtract <$ string "-" <|> 
      SEBinary p Multiply <$ string "*" <|> 
      SEBinary p Divide <$ string "/" <|> 
      SEBinary p GreaterEql <$ string ">=" <|> 
      SEBinary p LessEql <$ string "<=" <|> 
      SEBinary p Eql <$ string "==" <|> 
      SEBinary p NotEql <$ string "!=" <|>
      SEBinary p Greater <$ string ">" <|> 
      SEBinary p Less <$ string "<"

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
   p <- getCurrentPos
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
   pure $ SEFunc Func {funcIdent = ident, funcPos = p, funcArgs = args, funcReturnType = retTy, funcBody = block}
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
  p <- getCurrentPos
  ident <- parseIdent
  hspace1
  args <- parseCallArgs
  pure $ SEFuncCall p ident args
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
    scn = L.space space1 (L.skipLineComment ";") empty

parseFile :: FilePath -> T.Text -> Either (ParseErrorBundle T.Text E.PError) Program
parseFile f fc = do
  case (parse parseProgram f fc) of
    (Left err) -> Left err
    (Right v) -> Right $ Program v

parseAndPP :: FilePath -> IO ()
parseAndPP f = do
  fc <- readFile f >>= pure . T.pack
  case (parse parseProgram f fc) of
    (Left err) -> putStrLn $ errorBundlePretty err
    (Right v) -> putStrLn $ foldMap show v

someFunc :: IO ()
someFunc = putStrLn "this is someFunc from Liz/Parser.hs"
