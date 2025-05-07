{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}

module Parser (someFunc) where

import Data.String ( IsString (..))
import qualified Data.Text as T
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

default IsString (T.Text)
type Parser = Parsec Void T.Text
type Identifier = T.Text
newtype Value = Str T.Text
  | Integer Int
  | Float Double
  | Boolean Bool

someFunc :: IO ()
someFunc = putStrLn "this is someFunc from Parser.hs"
