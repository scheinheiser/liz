{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Common.Types where

import qualified Data.Text as T

-- import Text.Printf (printf)
-- import Data.List (intercalate)
import Text.Megaparsec (Pos)

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
  | Eql 
  | NotEql
  | GreaterEql
  | LessEql
  | Concat
  deriving (Show, Eq)

data UnaryOp = Negate 
  | Not
  deriving (Show, Eq)

data Arg = Arg
  { argIdent  :: T.Text
  , argType   :: Type
  } deriving (Show, Eq)

data Func = Func 
  { funcIdent       :: T.Text
  , funcPos         :: LizPos
  , funcArgs        :: [Arg]
  , funcReturnType  :: Type
  , funcBody        :: [SExpr]
  } deriving (Show, Eq)

data Var = Var
  { varIdent    :: T.Text
  , varType     :: Type
  , varValue    :: SExpr
  } deriving (Show, Eq)

type LizPos = (Pos, Pos)

-- TODO: make a pretty printing function for this.
data SExpr = SEIdentifier LizPos T.Text
  | SELiteral   LizPos T.Text
  | SEComment
  | SEFunc      Func
  | SEFuncCall  LizPos T.Text [SExpr] -- ident - values
  | SEReturn    LizPos SExpr
  | SEPrint     LizPos SExpr
  | SEType      Type
  | SEVar       LizPos Var
  | SEConst     LizPos Var
  | SESet       LizPos T.Text SExpr -- ident - value
  | SEBinary    LizPos BinaryOp SExpr SExpr
  | SEUnary     LizPos UnaryOp SExpr
  deriving (Show, Eq)
