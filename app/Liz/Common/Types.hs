{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Common.Types where

import qualified Data.Text as T
import Text.Megaparsec (Pos)

data Type = Int'
  | Float'
  | String'
  | Char'
  | Bool'
  | Unit'
  | Undef'
  deriving (Eq)

instance Show Type where
  show Int' = "Int"
  show Float' = "Float"
  show String' = "String"
  show Char' = "Char"
  show Bool' = "Bool"
  show Unit' = "Unit"
  show Undef' = "Undefined"

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
  , funcStart       :: LizPos
  , funcEnd         :: LizPos
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

newtype Program = Program [SExpr]
  deriving (Show, Eq)

-- TODO: make a pretty printing function for this.
data SExpr = SEIdentifier T.Text LizPos LizPos
  | SELiteral   T.Text LizPos LizPos
  | SEComment
  | SEFunc      Func
  | SEFuncCall  LizPos LizPos T.Text [SExpr] -- ident - values
  | SEReturn    LizPos LizPos SExpr
  | SEPrint     LizPos LizPos SExpr
  | SEType      Type
  | SEVar       LizPos LizPos Var
  | SEConst     LizPos LizPos Var
  | SESet       LizPos LizPos T.Text SExpr -- ident - value
  | SEBinary    BinaryOp LizPos LizPos SExpr SExpr
  | SEUnary     UnaryOp LizPos LizPos SExpr
  deriving (Show, Eq)
