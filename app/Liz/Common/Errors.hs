{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Liz.Common.Errors where

import qualified Data.Text as T

import Liz.Common.Types
import Text.Printf (printf)
import Text.Megaparsec

data SemErr = IncorrectType LizPos LizPos Type Type -- expected type ; given type
  | IncorrectTypes LizPos LizPos T.Text [Type] -- expected types ; given type
  | FailedLitInference LizPos LizPos T.Text
  | UndefinedIdentifier LizPos LizPos T.Text
  | IdentifierAlreadyInUse LizPos LizPos T.Text
  | AssigningToConstant LizPos LizPos T.Text
  | AssigningToFunction LizPos LizPos T.Text
  | NotEnoughArgs LizPos LizPos T.Text Int
  | TooManyArgs LizPos LizPos T.Text Int
  | IncorrectArgTypes LizPos LizPos T.Text [Type] [Type] -- expected types ; given types
  | NoEntrypoint
  | MultipleEntrypoints
  | NotImplemented SExpr
  deriving (Show, Eq)

data PError = FailedTypeInference T.Text
  | ReservedIdent T.Text
  | UnsupportedDeclaration T.Text
  | InferredUndefined
  | InvalidNumber T.Text
  | TooManyExprsIf
  | WrongArgCount Int Int -- expected - got
  deriving (Show, Eq, Ord)

instance ShowErrorComponent PError where
  showErrorComponent = \case
    ReservedIdent s ->          printf "[ERROR] Expected identifier, found keyword '%s'" (T.unpack s)
    FailedTypeInference s ->    printf "[ERROR] Failed to infer type of '%s'" (T.unpack s)
    UnsupportedDeclaration s -> printf "[ERROR] This type of declaration is currently not supported - '%s'" (T.unpack s)
    InferredUndefined ->        printf "[ERROR] Can't infer the type of undefined."
    InvalidNumber s ->          printf "[ERROR] The format of this number '%s' is invalid." (T.unpack s)
    TooManyExprsIf ->           printf "[ERROR] There are too many expressions within this if statement."
    WrongArgCount ex got ->     printf "[ERROR] Expected %i arg(s), but got %i arg(s)." ex got
