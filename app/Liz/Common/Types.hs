{-# LANGUAGE OverloadedStrings #-}

module Liz.Common.Types where

import qualified Data.Text as T
import Prettyprinter (Pretty (..))

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
  , funcPos         :: LizRange
  , funcArgs        :: [Arg]
  , funcReturnType  :: Type
  , funcBody        :: [SExpr]
  } deriving (Show, Eq)

data Var = Var
  { varIdent    :: T.Text
  , varType     :: Type
  , varValue    :: Expression
  } deriving (Show, Eq)

-- TODO: expand the macro system to be able to pass in values, similar to Lisp macros
data Macro = Macro
  { macPos   :: LizRange
  , macIdent :: T.Text
  , macValue :: Expression
  } deriving (Show, Eq)

data LizRange = LizRange !Int !Int
  deriving (Show, Eq)
 
newtype Program = Program [SExpr]
  deriving (Show, Eq)

data Expression = EBinary BinaryOp LizRange Expression Expression
  | EUnary      UnaryOp LizRange Expression
  | EReturn     LizRange Expression
  | EPrint      LizRange Expression
  | EFuncCall   LizRange T.Text [Expression] -- ident - values
  | ELiteral    Type T.Text LizRange
  | EIdentifier T.Text LizRange
  | EValueMacro T.Text LizRange
  deriving (Show, Eq)

data ControlFlow = FFunc Func
  | FBlockStmt LizRange [SExpr]
  | FIfStmt    LizRange Expression SExpr (Maybe SExpr) -- cond - truebranch - optional falsebranch
  deriving (Show, Eq)

data SExpr = SEComment
  | SEExpr      Expression
  | SEVar       LizRange Var
  | SEConst     LizRange Var
  | SESet       LizRange T.Text Expression
  | SEFlow      ControlFlow
  | SEMacroDef  Macro
  | SEType      Type
  deriving (Show, Eq)
