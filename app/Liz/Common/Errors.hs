{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Liz.Common.Errors where

import qualified Data.Text as T

import Liz.Common.Types
import Text.Printf (printf)
import Text.Megaparsec

data SemErr = IncorrectType LizRange Type Type -- expected type ; given type
  | IncorrectTypes LizRange T.Text [Type] -- expected types ; given type
  | FailedLitInference LizRange T.Text
  | UndefinedIdentifier LizRange T.Text
  | IdentifierAlreadyInUse LizRange T.Text
  | AssigningToConstant LizRange T.Text
  | AssigningToFunction LizRange T.Text
  | NotEnoughArgs LizRange T.Text Int
  | TooManyArgs LizRange T.Text Int
  | IncorrectArgTypes LizRange T.Text [Type] [Type] -- expected types ; given types
  | NoEntrypoint
  | MultipleEntrypoints
  | NotImplemented SExpr
  | InvalidArgType LizRange T.Text Type
  | RecursiveMacroDef LizRange T.Text
  | NonGlblMacroDef LizRange
  | InvalidBinaryExpr LizRange
  | InvalidUnaryExpr LizRange
  | InvalidIfStmt LizRange
  deriving (Show, Eq)

data PError = FailedTypeInference T.Text
  | ReservedIdent T.Text
  | UnsupportedDeclaration T.Text
  | InvalidNumber T.Text
  | TooManyExprsIf
  | WrongArgCount Int Int -- expected - got
  deriving (Show, Eq, Ord)

instance ShowErrorComponent PError where
  showErrorComponent = \case
    ReservedIdent s ->          printf "[ERROR] Expected identifier, found keyword '%s'" (T.unpack s)
    FailedTypeInference s ->    printf "[ERROR] Failed to infer type of '%s'" (T.unpack s)
    UnsupportedDeclaration s -> printf "[ERROR] This type of declaration is currently not supported - '%s'" (T.unpack s)
    InvalidNumber s ->          printf "[ERROR] The format of this number '%s' is invalid." (T.unpack s)
    TooManyExprsIf ->           printf "[ERROR] There are too many expressions within this if statement."
    WrongArgCount ex got ->     printf "[ERROR] Expected %i arg(s), but got %i arg(s)." ex got
