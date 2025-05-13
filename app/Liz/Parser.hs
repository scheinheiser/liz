{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}

module Liz.Parser where

import qualified Liz.Error as E
import qualified Data.Text as T

import Data.String ( IsString (..))
import Data.Char (isAlphaNum, isDigit, isLetter)
import Control.Applicative (liftA3)
import Control.Monad (join)
-- import Data.Void (Void)

import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L

default IsString (T.Text)
type Parser = Parsec E.PError T.Text

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

data SExpr = SEIdentifier T.Text
  | SELiteral T.Text
  | SEReturn SExpr
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
lizReserved = ["var", "set", "const", "if", "func", "return", "False", "True", "undefined", "not", "negate"]

fromLiteral :: T.Text -> Parser SExpr
fromLiteral t = case t of
  "Int"     -> pure $ SEType Int'
  "Float"   -> pure $ SEType Float'
  "String"  -> pure $ SEType String'
  "Char"    -> pure $ SEType Char'
  "Bool"    -> pure $ SEType Bool'
  "Unit"      -> pure $ SEType Unit'
  _ -> invalidType t

-- helper error functions
failedTypeInference :: T.Text -> Parser a
failedTypeInference = customFailure . E.FailedTypeInference

invalidType :: T.Text -> Parser a
invalidType = customFailure . E.InvalidType

reservedIdent :: T.Text -> Parser a
reservedIdent = customFailure . E.ReservedIdent

unsupportedDeclaration :: T.Text -> Parser a
unsupportedDeclaration = customFailure . E.UnsupportedDeclaration

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

parseBinaryOp' :: Parser T.Text
parseBinaryOp' = 
  choice [string "+", string "-", string "*", string "/", string ">=", string "<=", string "==", string "!=", string ">", string "<"]

parseUnaryOp :: Parser T.Text
parseUnaryOp = string "not" <|> string "negate"

parseValue :: Parser T.Text
parseValue = choice [parseStr, parseChar, parseNum, parseBool, parseUnit]

parseNested :: Parser SExpr
parseNested = (SELiteral <$> parseValue) <|> (SEIdentifier <$> parseIdent) <|> parseSExpr

-- main parsing functions
parseIdent :: Parser T.Text
parseIdent = do
  s <- letterChar
  r <- takeWhileP (Just "alphanumeric character, '-' or '_'") valid
  pure $ T.pack [s] <> r
  where
    valid :: Char -> Bool 
    valid = liftA3 (\x y z -> x || y || z) isAlphaNum ((==) '_') ((==) '-')

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
    valid = liftA2 (||) isAlphaNum ((==) ' ')

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
    valid = liftA2 (||) isDigit ((==) '.') 

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
  k <- string "var" <|> string "const"
  hspace1
  ident <- parseIdent
  if ident `elem` lizReserved
  then reservedIdent ident
  else do
    hspace1 
    ty <- (join $ fromLiteral <$> parseFromList lizTypes) <|> parseNested
    aux k ident ty
  where
    aux :: T.Text -> T.Text -> SExpr -> Parser SExpr
    aux decl iden ty@(SEType _) = do
      hspace1
      value <- parseNested
      pure $ (pickDecl decl) iden ty value
    aux decl iden expr@(SEBinary op _ _)  = (fromBinaryOp op) >>= \ty -> pure $ (pickDecl decl) iden ty expr
    aux decl iden expr@(SEUnary op _)     = (fromUnaryOp op) >>= \ty -> pure $ (pickDecl decl) iden ty expr
    aux decl iden lit@(SELiteral literal) = do
      ty <- join $ fromLiteral <$> inferType literal
      pure $ (pickDecl decl) iden ty lit
    aux _ _ op = unsupportedDeclaration $ T.show op

    --TODO: change to allow for floats to access numeric ops 
    -- It should correctly choose between int or float based on the lit.
    fromBinaryOp :: BinaryOp -> Parser SExpr
    fromBinaryOp op
      | op `elem` numeric = fromLiteral "Int"
      | op `elem` boolean = fromLiteral "Bool"
      where
        numeric = [Add, Subtract, Multiply, Divide]
        boolean = [Greater, Less, Equal, NotEql, GreaterEql, LessEql]

    fromUnaryOp :: UnaryOp -> Parser SExpr
    fromUnaryOp op 
      | op == Negate = fromLiteral "Int"
      | op == Not = fromLiteral "Bool"

    pickDecl decl
      | decl == "var" = SEVar
      | decl == "const" = SEConst

    inferType :: T.Text -> Parser T.Text
    inferType v
      | (count '.' v) == 1 =
        -- remove the dot to check if the rest are nums.
        if T.foldl' isDigitText True $ T.filter ((/=) '.') v
        then pure "Float" 
        else failedTypeInference v
      | T.foldl' isDigitText True v = pure "Int"
      | (T.take 1 v) == "'" && (T.last v) == '\'' = pure "Char"
      | (T.take 1 v) == "\"" && (T.last v) == '"' = pure "String"
      | v == "True" || v == "False" = pure "Bool"
      | v == "()" = pure "Unit"
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
  if ident `elem` lizReserved
  then reservedIdent ident
  else do
    hspace1
    value <- parseNested
    pure $ SESet ident value

parseUnary :: Parser SExpr
parseUnary = do
  op <- parseUnaryOp
  let op' = pickUnaryOp $ T.unpack op
  hspace1
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
  hspace1
  left <- parseNested
  hspace1
  right <- parseNested
  pure $ SEBinary op' left right
  where
    pickBinaryOp :: String -> BinaryOp
    pickBinaryOp c 
      | c == "+" = Add
      | c == "-" = Subtract
      | c == "*" = Multiply
      | c == "/" = Divide
      | c == ">=" = GreaterEql
      | c == "<=" = LessEql
      | c == "==" = Equal
      | c == "!=" = NotEql
      | c == ">" = Greater
      | c == "<" = Less

parseSExpr :: Parser SExpr
parseSExpr = between (char '(') (char ')') $ choice [parseBinary, parseUnary, parseVarDecl, parseSetStmt]

someFunc :: IO ()
someFunc = putStrLn "this is someFunc from Liz/Parser.hs"
