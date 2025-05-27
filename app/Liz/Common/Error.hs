{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE LambdaCase #-}

module Liz.Common.Error (PError (..), SemErr (..)) where

import qualified Data.Text as T

import Liz.Common.Types
import Text.Printf (printf)
import Text.Megaparsec

data SemErr = MismatchedTypes LizPos Type T.Text -- expected type ; given type
  | IncorrectType LizPos Type Type -- expected type ; given type
  | IncorrectTypes LizPos T.Text [Type] -- expected types ; given type
  | FailedLitInference LizPos T.Text
  | UndefinedIdentifier LizPos T.Text
  | UndefinedFunction LizPos T.Text
  | IdentifierAlreadyInUse LizPos T.Text
  | AssigningToConstant LizPos T.Text
  | AssigningToFunction LizPos T.Text
  | NotAFunction LizPos T.Text
  | NotEnoughArgs LizPos T.Text
  | TooManyArgs LizPos T.Text
  | IncorrectArgTypes LizPos T.Text [Type]
  | NotImplemented SExpr
  deriving (Show, Eq)

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
