{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Liz.Common.Types where

import qualified Data.Text as T

import Text.Printf (printf)
import Data.List (intercalate)
import Text.Megaparsec (Pos, unPos)

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
  } deriving (Eq)

instance Show Arg where
  show (Arg {..}) = printf "%v: %v" argIdent (show argType)

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
  } deriving (Eq)

type LizPos = (Pos, Pos)

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
  deriving (Eq)

--TODO: change this to an actual pretty print, using show messes with ghci shennanigans.
instance Show SExpr where
  show (SEIdentifier _ iden) = show iden
  show (SELiteral (_) lit) = printf "%v" lit
  show (SEUnary (line, col) op r) = printf "%v:%v %v %v" (unPos line) (unPos col) (show op) (show r)
  show (SEBinary (line, col) op l r) = printf "%v:%v %v %v %v" (unPos line) (unPos col) (show l) (show op) (show r)
  show (SEVar (line, col) Var{..}) = printf "%v:%v var %v: %v = %v\n" (unPos line) (unPos col) varIdent (show varType) (show varValue)
  show (SEConst (line, col) Var{..}) = printf "%v:%v const %v: %v = %v\n" (unPos line) (unPos col) varIdent (show varType) (show varValue)
  show (SESet (line, col) ident v) = printf "%v:%v set %v = %v\n" (unPos line) (unPos col) ident (show v)
  show (SEReturn (line, col) v) = printf "%v:%v ret( %v )\n" (unPos line) (unPos col) (show v)
  show (SEPrint (line, col) v) = printf "%v:%v print( %v )\n" (unPos line) (unPos col) (show v)
  show (SEFuncCall (line, col) ident l) = printf "%v:%v %v( %v )" (unPos line) (unPos col) ident (foldMap show l)
  show (SEFunc Func{..}) = printf "%v:%v func %v(%v) -> %v {\n%v}\n" (unPos $ fst funcPos) (unPos $ snd funcPos) funcIdent (intercalate ", " $ map show funcArgs) (show funcReturnType) (concatMap ((<>) "  " . show) funcBody)
  show (SEType ty) = printf "%v" (show ty)
  show SEComment = ""
