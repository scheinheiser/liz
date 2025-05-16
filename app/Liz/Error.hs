{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}

module Liz.Error where

import qualified Data.Text as T
import Text.Printf (printf)
import Text.Megaparsec

-- data CompilerError = ParserError PError
--   deriving (Show, Eq, Ord)

data PError = FailedTypeInference T.Text
  | ReservedIdent T.Text
  | UnsupportedDeclaration T.Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent PError where
  showErrorComponent = \v -> case v of
    FailedTypeInference s -> printf "[ERROR] Failed to infer type of '%s'" (T.unpack s)
    ReservedIdent s -> printf "[ERROR] Expected identifier, found keyword '%s'" (T.unpack s)
    UnsupportedDeclaration s -> printf "[ERROR] This type of declaration is currently not supported - '%s'" (T.unpack s)
