{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}

module Liz.Parser where

import Data.String ( IsString (..))
import Data.Char (isAlphaNum, isDigit, isLetter)

import qualified Data.Text as T
import Text.Printf (printf)

import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void (Void)

default IsString (T.Text)
type Parser = Parsec Void T.Text

data Type = Int'
  | Float'
  | String'
  | Char'
  | Bool'
  | Unit'
  deriving (Show, Eq)

data BinaryOp = Add
  | Subtract
  | Mulitply
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

data SExpr = SEIdentifier T.Text
  | SELiteral T.Text
  -- | SEFunc Func
  | SEType Type
  | SEVar T.Text SExpr SExpr -- ident - type - value
  | SEConst T.Text SExpr SExpr
  | SESet T.Text SExpr -- ident - value
  | SEBinary BinaryOp SExpr SExpr
  | SEUnary UnaryOp SExpr
  deriving (Show, Eq)

lizTypes :: [T.Text]
lizTypes = ["Int", "Float", "String", "Char", "Bool", "Unit"]

lizReserved :: [T.Text]
lizReserved = ["var", "set", "const", "if", "func", "False", "True", "undefined", "not", "negate"]

fromLiteral :: T.Text -> Parser SExpr
fromLiteral t = case t of
  "Int"     -> pure $ SEType Int'
  "Float"   -> pure $ SEType Float'
  "String"  -> pure $ SEType String'
  "Char"    -> pure $ SEType Char'
  "Bool"    -> pure $ SEType Bool'
  "Unit"      -> pure $ SEType Unit'
  _ -> fail "Reached unreachable in 'fromLiteral'."

-- helper parsing functions
parseFromList :: [T.Text] -> Parser T.Text
parseFromList = foldr1 (<|>) . map (string)

parseBinaryOp :: Parser T.Text 
parseBinaryOp = string "+" <|> 
  string "-" <|> 
  string "*" <|> 
  string "/" <|> 
  string ">=" <|> 
  string "<=" <|> 
  string "==" <|> 
  string "!=" <|>
  string ">" <|> 
  string "<"

parseUnaryOp :: Parser T.Text
parseUnaryOp = string "not" <|> string "negate"

parseValue :: Parser T.Text
parseValue = parseStr <|> parseChar <|> parseNum <|> parseBool

parseNested :: Parser SExpr
parseNested = parseSExpr  <|> (parseValue >>= \v -> pure $ SELiteral v) <|> (parseIdent >>= \i -> pure $ SEIdentifier i)

-- main parsing functions
parseIdent :: Parser T.Text
parseIdent = do
  i <- (:) <$> letterChar <*> (some $ alphaNumChar <|> char '-' <|> char '_')
  pure $ T.pack i

parseStr :: Parser T.Text
parseStr = do
  d1 <- char '"'
  str <- takeWhile1P (Just "alpha numeric character.") valid 
  d2 <- char '"'
  pure $ (d1 T.:< str) T.:> d2
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isAlphaNum ((==) ' ')

parseChar :: Parser T.Text
parseChar = do
  d1 <- char '\''
  c <- printChar
  d2 <- char '\''
  pure $ T.pack [d1, c, d2]

parseNum :: Parser T.Text
parseNum = do
  n <- takeWhile1P (Just "digits 0-9 or '.'.") valid
  pure n
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isDigit ((==) '.') 

parseBool :: Parser T.Text
parseBool = do
  b <- takeWhile1P (Just "letter") isLetter
  pure b

-- (var *ident* *type* *value*)
-- (var hello String "World")
-- (var four 4) ; inferred Int
parseVarDecl :: Parser SExpr
parseVarDecl = do
  k <- string "var" <|> string "const"
  _ <- char ' ' 
  ident <- parseIdent
  if ident `elem` lizReserved
  then fail $ printf "Expected identifier, found keyword '%s'" ident
  else do
    _ <- char ' ' 
    ty <- try $ (parseFromList lizTypes) <|> (lookAhead parseValue)
    aux k ident ty
  where
    aux decl identifier typeOrVal
      | typeOrVal == "Unit" = do
        _ <- char ' '
        u <- string "()"
        pure $ (pickDecl decl) identifier (SEType Unit') (SELiteral u)
      | typeOrVal `elem` lizTypes = do
        _ <- char ' '
        value <- parseNested
        ty <- fromLiteral typeOrVal
        pure $ (pickDecl decl) identifier ty value
      | otherwise = do
        (inferType $ T.unpack typeOrVal) >>= \ty -> do 
          value <- parseNested
          t <- fromLiteral ty
          pure $ (pickDecl decl) identifier t value

    inferType :: String -> Parser T.Text
    inferType v
      | (and . map isDigit) v = pure "Int"
      | (take 1 v) == "\"" = pure "String"
      | (take 1 v) == "'" = pure "Char"
      | v == "True" || v == "False" = pure "Bool"
      | (count '.' v) == 1 = pure "Float"
      | otherwise = fail $ printf "Failed to infer the type of %s" v

    pickDecl decl
      | decl == "var" = SEVar
      | decl == "const" = SEConst

    count :: Char -> String -> Int
    count _ [] = 0
    count y (x : xs) = if y == x then 1 + (count y xs) else count y xs

parseSetStmt :: Parser SExpr
parseSetStmt = do
  _ <- string "set"
  _ <- char ' '
  ident <- parseIdent
  _ <- char ' '
  value <- parseNested
  pure $ SESet ident value

parseUnary :: Parser SExpr
parseUnary = do
  op <- parseUnaryOp
  let op' = pickUnaryOp $ T.unpack op
  _ <- char ' '
  v <- parseNested
  pure $ SEUnary op' v
  where
    pickUnaryOp :: String -> UnaryOp
    pickUnaryOp c 
      | c == "not" = Not
      | c == "negate" = Negate

parseBinary :: Parser SExpr
parseBinary = do
  op <- parseBinaryOp
  let op' = pickBinaryOp $ T.unpack op
  _ <- char ' '
  left <- parseNested
  _ <- char ' '
  right <- parseNested
  pure $ SEBinary op' left right
  where
    pickBinaryOp :: String -> BinaryOp
    pickBinaryOp c 
      | c == "+" = Add
      | c == "-" = Subtract
      | c == "*" = Mulitply
      | c == "/" = Divide
      | c == ">=" = GreaterEql
      | c == "<=" = LessEql
      | c == "==" = Equal
      | c == "!=" = NotEql
      | c == ">" = Greater
      | c == "<" = Less

parseSExpr :: Parser SExpr
parseSExpr = between (char '(') (char ')') $ parseBinary <|> parseUnary <|> parseVarDecl <|> parseSetStmt

someFunc :: IO ()
someFunc = putStrLn "this is someFunc from Liz/Parser.hs"
