{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}

module Liz.Parser where

import Data.String ( IsString (..))
import Data.Char (isAlphaNum, isDigit)
import Control.Applicative (liftA3)
import qualified Data.Text as T
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

default IsString (T.Text)
type Parser = Parsec Void T.Text
type Identifier = T.Text

data BinOp = Plus
  | Minus
  | Mulitply
  | Divide
  deriving (Show, Eq)

data UnOp = Negate 
  | Not
  deriving (Show, Eq)

data SExpr = SEIdentifier T.Text
  | SEString T.Text
  | SEChar T.Text
  | SEInt T.Text
  | SEFloat T.Text
  -- | SEFunc Func
  | SEType T.Text
  | SEBinary BinOp SExpr SExpr
  | SEUnary UnOp SExpr
  deriving (Show, Eq)

seTypes :: [T.Text]
seTypes = ["Int", "Float", "String", "Char", "Bool"]

seReserved :: [T.Text]
seReserved = ["var", "set", "const", "if", "func"]

parseKeywordFromList :: [T.Text] -> Parser SExpr
parseKeywordFromList = (pure . SEType =<<) . aux
  where
    aux :: [T.Text] -> Parser T.Text
    aux xs = foldr1 (<|>) $ string <$> xs

parseIdent :: Parser SExpr
parseIdent = do
  i <- (:) <$> letterChar <*> (some $ alphaNumChar <|> char '-' <|> char '_')
  return $ SEIdentifier (T.pack i)

parseStr :: Parser SExpr 
parseStr = do
  _ <- char '"'
  str <- takeWhile1P (Just "alpha numeric character.") valid 
  _ <- char '"'
  return $ SEString str
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isAlphaNum ((==) ' ')

parseChar :: Parser SExpr
parseChar = do
  _ <- char '\''
  c <- printChar
  _ <- char '\''
  (return . SEChar) $ T.pack [c]

parseNum :: Parser SExpr
parseNum = do
  n <- takeWhile1P (Just "digits 0-9 or '.'.") valid
  if '.' `elem` (T.unpack n)
  then return $ SEFloat n
  else return $ SEInt n
  where
    valid :: Char -> Bool
    valid = liftA2 (||) isDigit ((==) '.') 

-- (var *ident* *optional type* *value*)
-- (var hello String "World")
-- (var four 4) ; inferred Int
parseVarDecl :: Parser SExpr
parseVarDecl = do
  k <- string "var" <|> string "const"
  hspace1
  ty <- lookAhead $ (parseKeywordFromList seTypes) <|> parseIdent
  return $ SEIdentifier "unfinished"

parseSExpr :: Parser SExpr
parseSExpr = undefined

someFunc :: IO ()
someFunc = putStrLn "this is someFunc from Liz/Parser.hs"
