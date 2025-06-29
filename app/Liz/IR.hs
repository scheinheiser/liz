{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OrPatterns #-}

module Liz.IR where

import qualified Liz.Common.Types as L
-- import qualified Liz.Common.Errors as E
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

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
  | IRFunc T.Text [L.Arg] [IROp] L.Type -- identifier - exprs - return type
  | IRIf IROp (Goto, Label) (Maybe (Goto, Label)) -- cond - true branch - optional false branch
  | IRLabel Label -- a wrapper around the label for the leader algorithm
  | IRGoto Goto -- a wrapper around goto for control flow/leader algorithm
  deriving Show

newtype Label = Label (T.Text, [IROp]) -- name, expressions
  deriving Show

type Goto = T.Text

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

pushExpr :: Label -> IROp -> Label
pushExpr (Label (n, exprs)) instr = Label (n, reverse (instr : exprs))

translateBody :: [L.SExpr] -> Int -> ([IROp], Int)
translateBody l idx = let (res, i) = aux l idx [] in (reverse res, i)
  where
    aux :: [L.SExpr] -> Int -> [IROp] -> ([IROp], Int)
    aux [] i acc = (acc, i)
    aux (x : xs) i acc = let (op, ni) = fromSExpr x i in aux xs ni (op : acc)

tempVarIdent :: Int -> T.Text
tempVarIdent i = T.pack $ "t" <> (show i)

-- main ir functions
fromSExpr :: L.SExpr -> Int -> (IROp, Int)
fromSExpr (L.SELiteral ty lit _ _) i =
  let 
    v = 
      case ty of
        L.Int' -> IRInt $ read @Int (T.unpack lit)
        L.Float' -> IRFloat $ read @Double (T.unpack lit)
        L.Bool' -> IRBool $ read @Bool (T.unpack lit)
        L.Char' -> IRChar $ read @Char (T.unpack lit)
        L.Unit' -> IRUnit
        L.Undef' -> IRUndef
        L.String' -> IRString lit
  in (v, i)
fromSExpr (L.SEBinary op _ _ l r) i =
  let
    (el, ni) = fromSExpr l i
    (er, fi) = fromSExpr r ni
    irOp = 
      case op of
        L.Add -> IRBin L.Add
        L.Subtract -> IRBin L.Subtract
        L.Multiply -> IRBin L.Multiply
        L.Divide -> IRBin L.Divide
        L.Concat -> IRBin L.Concat
        L.GreaterEql -> IRBin L.GreaterEql
        L.LessEql -> IRBin L.LessEql
        L.Eql -> IRBin L.Eql
        L.NotEql -> IRBin L.NotEql
        L.Greater -> IRBin L.Greater
        L.Less -> IRBin L.Less
  in (IRVar (tempVarIdent $ fi + 1) (irOp el er), fi + 1)
fromSExpr (L.SEUnary op _ _ v) i =
  let
    (ev, ni) = fromSExpr v i
    irOp = 
      case op of
        L.Not -> IRUn L.Not
        L.Negate -> IRUn L.Negate
  in (IRVar (tempVarIdent $ ni + 1) (irOp ev), ni + 1)
fromSExpr (L.SEVar _ _ L.Var{varIdent=ident, varValue=val}) i = 
  let (evaluated_value, ni) = fromSExpr val i in ((IRVar ident evaluated_value), ni)
fromSExpr (L.SEConst _ _ L.Var{varIdent=ident, varValue=val}) i = 
  let (evaluated_value, ni) = fromSExpr val i in ((IRConst ident evaluated_value), ni)
fromSExpr (L.SEFunc L.Func{funcIdent=ident, funcArgs=args, funcReturnType=ret, funcBody=body}) i =
  let (tbody, ni) = translateBody body i in (IRFunc ident args tbody ret, ni)
fromSExpr (L.SESet _ _ ident val) i = 
  let (evaluated_value, ni) = fromSExpr val i in ((IRVar ident evaluated_value), ni)
fromSExpr (L.SEIdentifier ident _ _) i = (IRIdent ident, i)
fromSExpr (L.SEReturn _ _ v) i = 
  let (ev, ni) = fromSExpr v i in (IRRet ev, ni)
fromSExpr (L.SEPrint _ _ v) i = 
  let (ev, ni) = fromSExpr v i in (IRPrint ev, ni)
-- NOTE: when going onto if stmts, leave the goto at the of each branch blank.
-- they can be backpatched when leaders have been applied

-- FIX: some logic issue with the code means that label idx suffixes aren't carried over
applyLabels :: NE.NonEmpty IROp -> [IROp]
applyLabels prog = 
  let
    label_idx = 0
    first_lead = Label ((labelSuffix label_idx), [NE.head prog])
  in aux (NE.drop 1 prog) first_lead [] label_idx
  where
    labelSuffix :: Int -> T.Text
    labelSuffix i = T.pack $ "L" <> (show i)

    aux :: [IROp] -> Label -> [IROp] -> Int -> [IROp]
    aux [] curr_lbl@(Label (n, exprs)) acc _ = 
      if length exprs == 0 then acc
                           else (IRLabel curr_lbl) : acc
    aux ((IRFunc ident args fexprs ret) : rest) old_lbl acc i =
      let led_func = IRFunc ident args (applyLabels $ NE.fromList fexprs) ret in
      aux rest (Label (labelSuffix i, [])) (led_func : (IRLabel old_lbl) : acc) (i + 1)

    -- TODO: modify if stmt code to reflect the blank gotos? may or may not be necessary
    aux ((IRIf cond (goto, (Label (tname, texprs))) Nothing) : rest) old_lbl acc i =
      let 
        led_branch = applyLabels $ NE.fromList texprs 
        led_ifstmt = IRIf cond (goto, Label (tname, led_branch)) Nothing
      in aux rest (Label (labelSuffix i, [])) (led_ifstmt : (IRLabel old_lbl) : acc) (i + 1)

    aux ((IRIf cond (gototrue, (Label (tname, texprs))) (Just (gotofalse, (Label (fname, fexprs))))) : rest) old_lbl acc i =
      let 
        led_tbranch = applyLabels $ NE.fromList texprs 
        led_fbranch = applyLabels $ NE.fromList fexprs
        led_ifstmt = IRIf cond (gototrue, Label (tname, led_tbranch)) (Just (gotofalse, Label (fname, led_fbranch)))
      in aux rest (Label (labelSuffix i, [])) (led_ifstmt : (IRLabel old_lbl) : acc) (i + 1)

    aux (goto@(IRGoto _) : rest) old_lbl acc i = aux rest (Label (labelSuffix i, [])) (goto : (IRLabel old_lbl) : acc) (i + 1)
    aux (expr : rest) curr_lbl acc i = aux rest (pushExpr curr_lbl expr) acc (i + 1)

programToIR :: L.Program -> [IROp]
programToIR (L.Program sexprs) = applyLabels $ NE.fromList $ reverse $ naiveConv sexprs 0
  where
    naiveConv :: [L.SExpr] -> Int -> [IROp]
    naiveConv [] _ = []
    naiveConv (x : xs) i = let (instr, ni) = fromSExpr x i in instr : naiveConv xs ni

-- main optimisation functions
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
