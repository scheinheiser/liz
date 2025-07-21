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

data SExpr = SEIdentifier T.Text LizPos LizPos
  | SELiteral   Type T.Text LizPos LizPos
  | SEComment
  | SEFunc      Func
  | SEMacroDef  Macro
  | SEFuncCall  LizPos LizPos T.Text [SExpr] -- ident - values
  | SEMacroCall LizPos LizPos T.Text
  | SEBlockStmt LizPos LizPos [SExpr]
  | SEIfStmt    LizPos LizPos SExpr SExpr (Maybe SExpr) -- cond - truebranch - optional falsebranch
  | SEReturn    LizPos LizPos SExpr
  | SEPrint     LizPos LizPos SExpr
  | SEType      Type
  | SEVar       LizPos LizPos Var
  | SEConst     LizPos LizPos Var
  | SESet       LizPos LizPos T.Text SExpr -- ident - value
  | SEBinary    BinaryOp LizPos LizPos SExpr SExpr
  | SEUnary     UnaryOp LizPos LizPos SExpr
  deriving (Show, Eq)
