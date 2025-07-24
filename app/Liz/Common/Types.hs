{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Common.Types where

import qualified Data.Text as T
import Prettyprinter (Pretty (..))
import Text.Megaparsec (Pos)

-- TODO: add support for specific integer/float sizes (i.e. i32, i64, f32, f64)
data Type = Int'
  | Float'
  | String'
  | Char'
  | Bool'
  | Unit'
  deriving (Eq)

instance Show Type where
  show Int' = "Int"
  show Float' = "Float"
  show String' = "String"
  show Char' = "Char"
  show Bool' = "Bool"
  show Unit' = "Unit"

instance Pretty Type where
  pretty Int' = "int"
  pretty Float' = "float"
  pretty String' = "str"
  pretty Char' = "char"
  pretty Bool' = "bool"
  pretty Unit' = "unit"

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

-- TODO: expand the macro system to be able to pass in values, similar to Lisp macros
data Macro = Macro
  { macStart :: LizPos
  , macEnd   :: LizPos
  , macIdent :: T.Text
  , macValue :: SExpr
  } deriving (Show, Eq)

type LizPos = (Pos, Pos)

newtype Program = Program [SExpr]
  deriving (Show, Eq)

data Expression = EVar LizPos LizPos Var
  | EConst     LizPos LizPos Var
  | ESet       LizPos LizPos T.Text SExpr -- ident - value
  | EBinary    BinaryOp LizPos LizPos SExpr SExpr
  | EUnary     UnaryOp LizPos LizPos SExpr
  | EReturn    LizPos LizPos SExpr
  | EPrint     LizPos LizPos SExpr
  | EFuncCall  LizPos LizPos T.Text [SExpr] -- ident - values
  | ELiteral  Type T.Text LizPos LizPos
  | EIdentifier T.Text LizPos LizPos
  | EValueMacro T.Text LizPos LizPos
  deriving (Show, Eq)

data SExpr = SEComment
  | SEExpr      Expression
  | SEFunc      Func
  | SEMacroDef  Macro
  | SEBlockStmt LizPos LizPos [SExpr]
  | SEIfStmt    LizPos LizPos Expression SExpr (Maybe SExpr) -- cond - truebranch - optional falsebranch
  | SEType      Type
  deriving (Show, Eq)
