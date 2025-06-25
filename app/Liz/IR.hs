{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OrPatterns #-}

module Liz.IR where

import qualified Liz.Common.Types as L
-- import qualified Liz.Common.Errors as E
import qualified Data.Text as T

data Lit = LBool Bool 
  | LInt Int 
  | LFloat Double
  | LString T.Text
  | LChar Char 
  | LUnit
  | LUndef
  deriving Show

-- TODO: add 'goto' op when control flow is implemented
data IROp = IRInt Int
  | IRBool Bool 
  | IRFloat Double
  | IRString T.Text
  | IRChar Char 
  | IRUnit
  | IRUndef
  | IRIdent T.Text
  | IRBin L.BinaryOp IROp IROp
  | IRUn L.UnaryOp IROp
  | IRRet IROp
  | IRPrint IROp
  | IRVar T.Text IROp
  | IRConst T.Text IROp
  deriving Show

-- helper functions
fromComp :: (Eq a, Ord a) => L.BinaryOp -> (a -> a -> Bool)
fromComp L.Greater = (>)
fromComp L.Less = (<)
fromComp L.GreaterEql = (>=)
fromComp L.LessEql = (<=)
fromComp L.NotEql = (/=)
fromComp L.Eql = (==)

fromArith :: Num a => L.BinaryOp -> (a -> a -> a)
fromArith L.Multiply = (*)
fromArith L.Subtract = (-)
fromArith L.Add = (+)

-- main functions

-- TODO: Do a TAC-style thing where each expr is allocated to a temp variable.
-- Just accept some count for temp no.s, and allocate in binary/unary branch
fromSExpr :: L.SExpr -> IROp
fromSExpr (L.SELiteral ty lit _ _) =
  case ty of
    L.Int' -> IRInt $ read @Int (T.unpack lit)
    L.Float' -> IRFloat $ read @Double (T.unpack lit)
    L.Bool' -> IRBool $ read @Bool (T.unpack lit)
    L.Char' -> IRChar $ read @Char (T.unpack lit)
    L.Unit' -> IRUnit
    L.Undef' -> IRUndef
    L.String' -> IRString lit
fromSExpr (L.SEBinary op _ _ l r) =
  case op of
    L.Add -> IRBin L.Add (fromSExpr l) (fromSExpr r)
    L.Subtract -> IRBin L.Subtract (fromSExpr l) (fromSExpr r)
    L.Multiply -> IRBin L.Multiply (fromSExpr l) (fromSExpr r)
    L.Divide -> IRBin L.Divide (fromSExpr l) (fromSExpr r)
    L.Concat -> IRBin L.Concat (fromSExpr l) (fromSExpr r)
    L.GreaterEql -> IRBin L.GreaterEql (fromSExpr l) (fromSExpr r)
    L.LessEql -> IRBin L.LessEql (fromSExpr l) (fromSExpr r)
    L.Eql -> IRBin L.Eql (fromSExpr l) (fromSExpr r)
    L.NotEql -> IRBin L.NotEql (fromSExpr l) (fromSExpr r)
    L.Greater -> IRBin L.Greater (fromSExpr l) (fromSExpr r)
    L.Less -> IRBin L.Less (fromSExpr l) (fromSExpr r)
fromSExpr (L.SEUnary op _ _ v) =
  case op of
    L.Not -> IRUn L.Not (fromSExpr v)
    L.Negate -> IRUn L.Negate (fromSExpr v)
fromSExpr (L.SEVar _ _ L.Var{varIdent=i, varValue=val}) = IRVar i (fromSExpr val)
fromSExpr (L.SEConst _ _ L.Var{varIdent=i, varValue=val}) = IRConst i (fromSExpr val)
fromSExpr (L.SESet _ _ i val) = IRVar i (fromSExpr val)
fromSExpr (L.SEIdentifier i _ _) = IRIdent i
fromSExpr (L.SEReturn _ _ v) = IRRet $ fromSExpr v
fromSExpr (L.SEPrint _ _ v) = IRPrint $ fromSExpr v

foldExpr :: IROp -> IROp
foldExpr l@(IRInt _; IRFloat _; IRString _; IRChar _; IRBool _) = l
foldExpr op@(IRBin p@(L.Add; L.Subtract; L.Multiply) l r) =
  case (l, r) of
    (IRInt lop, IRInt rop) -> let arith = fromArith p in IRInt (arith lop rop)
    (IRFloat lop, IRFloat rop) -> let arith = fromArith p in IRFloat (arith lop rop)
    _ -> op
foldExpr op@(IRBin L.Divide l r) =
  case (l, r) of
    (IRInt lop, IRInt rop) -> IRInt (div lop rop)
    (IRFloat lop, IRFloat rop) -> IRFloat (lop / rop)
    _ -> op
foldExpr op@(IRBin p@(L.Greater; L.Less; L.LessEql; L.GreaterEql; L.NotEql; L.Eql) l r) =
  case (l, r) of
    (IRInt lop, IRInt rop) -> let comp = fromComp p in IRBool (comp lop rop)
    (IRFloat lop, IRFloat rop) -> let comp = fromComp p in IRBool (comp lop rop)
    (IRString lop, IRString rop) -> let comp = fromComp p in IRBool (comp lop rop)
    (IRChar lop, IRChar rop) -> let comp = fromComp p in IRBool (comp lop rop)
    (IRBool lop, IRBool rop) -> let comp = fromComp p in IRBool (comp lop rop)
    _ -> op
foldExpr op@(IRBin L.Concat l r) =
  case (l, r) of
    (IRString lop, IRString rop) -> IRString (lop <> rop)
    _ -> op
