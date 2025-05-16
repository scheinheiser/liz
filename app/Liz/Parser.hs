{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}

module Liz.Parser where

import qualified Liz.Error as E
import qualified Data.Text as T

import Data.String ( IsString (..))
import Data.Char (isAlphaNum, isDigit, isLetter)

import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

default IsString (T.Text)
type Parser = Parsec E.PError T.Text

scn :: Parser ()
scn = L.space space1 (L.skipLineComment ";") empty

data Type = Int'
  | Float'
  | String'
  | Char'
  | Bool'
  | Unit'
  deriving (Show, Eq)

data BinaryOp = Add
  | Subtract
  | Multiply
  | Divide
  | Greater
  | Less 
  | Equal 
  | NotEql
  | GreaterEql
  | LessEql
  deriving (Show, Eq)

data UnaryOp = Negate 
  | Not
  deriving (Show, Eq)

data Arg = Arg
  { argIdent :: T.Text
  , argType :: Type
  }
  deriving (Show, Eq)

data Func = Func 
  { funcIdent       :: T.Text
  , funcArgs        :: [Arg]
  , funcReturnType  :: Type
  -- , funcBody        :: [SExpr]
  }
  deriving (Show, Eq)

data SExpr = SEIdentifier T.Text
  | SELiteral T.Text
  | SEReturn SExpr
  | SEFunc Func
  | SEType Type
  | SEVar T.Text SExpr SExpr -- ident - type - value
  | SEConst T.Text SExpr SExpr
  | SESet T.Text SExpr -- ident - value
  | SEBinary BinaryOp SExpr SExpr
  | SEUnary UnaryOp SExpr
  deriving (Show, Eq)

lizReserved :: [T.Text]
lizReserved = 
  ["var", "set", "const", "if", "func", "return", "False",
   "True", "undefined", "not", "negate", "Int", "Float", 
   "String", "Char", "Bool"]

-- helper error functions
failedTypeInference :: T.Text -> Parser a
failedTypeInference = customFailure . E.FailedTypeInference

reservedIdent :: T.Text -> Parser a
reservedIdent = customFailure . E.ReservedIdent

unsupportedDeclaration :: T.Text -> Parser a
unsupportedDeclaration = customFailure . E.UnsupportedDeclaration

-- helper parsing functions
parseType :: Parser Type
parseType =
  Int' <$ string "Int" <|>
  Float' <$ string "Float" <|>
  String' <$ string "String" <|>
  Char' <$ string "Char" <|>
  Bool' <$ string "Bool" <|>
  Unit' <$ string "Unit"

parseValue :: Parser T.Text
parseValue = choice [parseStr, parseChar, parseNum, parseBool, parseUnit]

parseNested :: Parser SExpr
parseNested = (SELiteral <$> parseValue) <|> (SEIdentifier <$> parseIdent) <|> parseSExpr

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

parseStr :: Parser T.Text
parseStr = do
  d1 <- char '"'
  str <- takeWhile1P (Just "alphanumeric character.") valid 
  d2 <- char '"'
  pure $ (d1 T.:< str) T.:> d2
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isAlphaNum (' ' ==)

parseChar :: Parser T.Text
parseChar = do
  d1 <- char '\''
  c <- printChar
  d2 <- char '\''
  pure $ T.pack [d1, c, d2]

parseNum :: Parser T.Text
parseNum = do
  n <- (takeWhile1P (Just "digits 0-9 or '.'") valid) <* notFollowedBy letterChar
  pure n
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isDigit ('.' ==) 

parseBool :: Parser T.Text
parseBool = do
  b <- (takeWhile1P (Just "letter") isLetter) <* notFollowedBy digitChar
  pure b

{-
  (var *ident* *type* *value*)
  (var hello String "World")
  (var four 4) ; inferred Int 
-}
parseVarDecl :: Parser SExpr
parseVarDecl = do
  decType <- SEVar <$ string "var" <|> SEConst <$ string "const"
  hspace1
  ident <- parseIdent
  hspace1 
  ty <- (parseType >>= pure . SEType) <|> parseNested
  aux decType ident ty
  where
    aux decl iden ty@(SEType _) = do
      hspace1
      value <- parseNested
      pure $ decl iden ty value
    aux decl iden lit@(SELiteral literal) = do
      ty <- inferType literal
      pure $ decl iden ty lit
    aux _ _ op = unsupportedDeclaration $ T.show op

    inferType :: T.Text -> Parser SExpr
    inferType v
      | (count '.' v) == 1 =
        if T.foldl' isDigitText True $ T.filter ((/=) '.') v
        then pure $ SEType Float'
        else failedTypeInference v
      | T.foldl' isDigitText True v = pure $ SEType Int'
      | (T.take 1 v) == "'" && (T.last v) == '\'' = pure $ SEType Char'
      | (T.take 1 v) == "\"" && (T.last v) == '"' = pure $ SEType String'
      | v == "True" || v == "False" = pure $ SEType Bool'
      | v == "()" = pure $ SEType Unit'
      | otherwise = failedTypeInference v
        where
          isDigitText :: Bool -> Char -> Bool
          isDigitText = \acc c -> if isDigit c then (True || acc) else (False || acc)

    count :: Char -> T.Text -> Int
    count t = (T.length . T.filter (t ==))

parseSetStmt :: Parser SExpr
parseSetStmt = do
  _ <- string "set"
  hspace1
  ident <- parseIdent
  hspace1
  value <- parseNested
  pure $ SESet ident value

parseUnary :: Parser SExpr
parseUnary = do
  op <- parseUnaryOp
  hspace1
  v <- parseNested
  pure $ op v
  where
    parseUnaryOp = SEUnary Not <$ string "not" <|> SEUnary Negate <$ string "negate"

parseBinary :: Parser SExpr
parseBinary = do
  op <- parseBinaryOp
  hspace1
  left <- parseNested
  hspace1
  right <- parseNested
  pure $ op left right
  where
    parseBinaryOp = SEBinary Add <$ string "+" <|> 
      SEBinary Subtract <$ string "-" <|> 
      SEBinary Multiply <$ string "*" <|> 
      SEBinary Divide <$ string "/" <|> 
      SEBinary GreaterEql <$ string ">=" <|> 
      SEBinary LessEql <$ string "<=" <|> 
      SEBinary Equal <$ string "==" <|> 
      SEBinary NotEql <$ string "!=" <|>
      SEBinary Greater <$ string ">" <|> 
      SEBinary Less <$ string "<"

{-
data Arg = Arg
  { argIdent :: T.Text
  , argType :: Type
  }

data Func = Func 
  { funcIdent ``    :: T.Text
  , funcArgs        :: [Arg]
  , funcReturnType  :: Type
  , funcBody        :: [SExpr]
  }

  (func *ident* *args* *return type* *body*)
  (func increment [number ~ Int] > Int
    (+ number 1)) ; the last executed sexpr's value is returned.
-}
parseFuncDecl :: Parser SExpr
parseFuncDecl = do
  -- p <- L.indentLevel >>= L.indentGuard scn GT
  _ <- string "func"
  hspace1
  ident <- parseIdent
  hspace1
  args <- parseFuncArgs
  hidden hspace
  _ <- char '>'
  hidden hspace
  retTy <- parseType
  pure $ SEFunc Func {funcIdent = ident, funcArgs = args, funcReturnType = retTy}
  where
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
          pure Arg { argIdent = ident, argType = ty}

parseSExpr :: Parser SExpr
parseSExpr = between (char '(') (char ')') $ choice [parseFuncDecl, parseBinary, parseUnary, parseVarDecl, parseSetStmt]

someFunc :: IO ()
someFunc = putStrLn "this is someFunc from Liz/Parser.hs"
