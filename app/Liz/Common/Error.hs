{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE LambdaCase #-}

module Liz.Common.Error (PError (..), SemErr (..)) where

import qualified Data.Text as T

import Liz.Common.Types (LizPos)
import Text.Printf (printf)
import Text.Megaparsec

data SemErr = MismatchedTypes LizPos T.Text
  | IncorrectType LizPos T.Text
  | FailedLitInference LizPos T.Text
  | UndefinedIdentifier LizPos T.Text
  deriving (Show, Eq, Ord)

data PError = FailedTypeInference T.Text
  | ReservedIdent T.Text
  | UnsupportedDeclaration T.Text
  | InferredUndefined
  deriving (Show, Eq, Ord)

instance ShowErrorComponent PError where
  showErrorComponent = \case
    ReservedIdent s ->          printf "[ERROR] Expected identifier, found keyword '%s'" (T.unpack s)
    FailedTypeInference s ->    printf "[ERROR] Failed to infer type of '%s'" (T.unpack s)
    UnsupportedDeclaration s -> printf "[ERROR] This type of declaration is currently not supported - '%s'" (T.unpack s)
    InferredUndefined ->      printf "[ERROR] Can't infer the type of undefined."
