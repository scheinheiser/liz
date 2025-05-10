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
  deriving (Show, Eq)

data BinOp = Plus
  | Minus
  | Mulitply
  | Divide
  deriving (Show, Eq)

data UnOp = Negate 
  | Not
  deriving (Show, Eq)

data SExpr = SELiteral T.Text
  -- | SEFunc Func
  | SEType Type
  | SEVar T.Text SExpr SExpr -- ident - type - value
  | SEConst T.Text SExpr SExpr
  | SEBinary BinOp SExpr SExpr
  | SEUnary UnOp SExpr
  deriving (Show, Eq)

lizTypes :: [T.Text]
lizTypes = ["Int", "Float", "String", "Char", "Bool"]

lizReserved :: [T.Text]
lizReserved = ["var", "set", "const", "if", "func", "False", "True", "undefined"]

fromLiteral :: T.Text -> Parser SExpr
fromLiteral t = case t of
  "Int"     -> pure $ SEType Int'
  "Float"   -> pure $ SEType Float'
  "String"  -> pure $ SEType String'
  "Char"    -> pure $ SEType Char'
  "Bool"    -> pure $ SEType Bool'
  _ -> fail "Reached unreachable in 'fromLiteral'."

parseFromList :: [T.Text] -> Parser T.Text
parseFromList = foldr1 (<|>) . map (string)

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

parseValue :: Parser T.Text
parseValue = parseStr <|> parseChar <|> parseNum <|> parseBool

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
    ty <- try $ (parseFromList lizTypes) <|> parseValue
    aux k ident ty
  where
    aux decl identifier typeOrVal
      | typeOrVal `elem` lizTypes = do
        _ <- char ' '
        value <- parseValue
        ty <- fromLiteral typeOrVal
        pure $ (pickDecl' decl) identifier ty (SELiteral value)
      | otherwise = do
        (inferType $ T.unpack typeOrVal) >>= \ty -> do 
          t <- fromLiteral ty
          pure $ (pickDecl' decl) identifier t (SELiteral typeOrVal)

    pickDecl' decl
      | decl == "var" = SEVar
      | decl == "const" = SEConst

    inferType :: String -> Parser T.Text
    inferType v
      | (and . map isDigit) v = pure "Int"
      | (take 1 v) == "\"" = pure "String"
      | (take 1 v) == "'" = pure "Char"
      | v == "True" || v == "False" = pure "Bool"
      | (count '.' v) == 1 = pure "Float"
      | otherwise = fail $ printf "Failed to infer the type of %s" v

    count :: Char -> String -> Int
    count _ [] = 0
    count y (x : xs) = if y == x then 1 + (count y xs) else count y xs

parseSExpr :: Parser SExpr
parseSExpr = undefined

someFunc :: IO ()
someFunc = putStrLn "this is someFunc from Liz/Parser.hs"
