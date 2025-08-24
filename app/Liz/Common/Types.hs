{-# LANGUAGE OverloadedStrings #-}

module Liz.Common.Types where

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
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

data Program = Program [Func] [GlblVar] [Macro]
  deriving (Show, Eq)

instance Semigroup Program where
  (<>) (Program f1 g1 m1) (Program f2 g2 m2) =
    Program (f1 <> f2) (g1 <> g2) (m1 <> m2)

instance Monoid Program where
  mempty = Program [] [] []

data Func = Func 
  { funcIdent       :: T.Text
  , funcPos         :: LizRange
  , funcArgs        :: [Arg]
  , funcReturnType  :: Type
  , funcBody        :: [SExpr]
  } deriving (Show, Eq)

data GlblVar = GlblVar LizRange Var
  deriving (Show, Eq)

data Expression = EBinary BinaryOp LizRange Expression Expression
  | EUnary      UnaryOp LizRange Expression
  | EReturn     LizRange Expression
  | EPrint      LizRange Expression
  | EFuncCall   LizRange T.Text [Expression] -- ident - values
  | ELiteral    Type T.Text LizRange
  | EFormat     LizRange T.Text [Expression] -- format string - values
  | EIdentifier T.Text LizRange
  | EValueMacro T.Text LizRange
  | EBreakStmt  LizRange T.Text
  deriving (Show, Eq)

data ControlFlow = FBlockStmt LizRange (NE.NonEmpty SExpr)
  | FIfStmt    LizRange Expression SExpr (Maybe SExpr) -- cond - truebranch - optional falsebranch
  | FUntilStmt LizRange (Maybe T.Text) Expression [SExpr] -- optional name - condition - loop body
  deriving (Show, Eq)

data SExpr = SEComment
  | SEExpr      Expression
  | SEVar       LizRange Var
  | SEConst     LizRange Var
  | SESet       LizRange T.Text Expression
  | SEFlow      ControlFlow
  deriving (Show, Eq)
