{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OrPatterns #-}

module Liz.IR.IR (ppIR, programToIR) where

import Liz.IR.IRTypes
import qualified Liz.Common.Types as L

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import qualified Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)

-- helper functions
pushExpr :: Label -> IROp -> Label
pushExpr (Label (n, exprs)) instr = Label (n, instr : exprs)

translateBody :: [L.SExpr] -> Int -> IR -> ([IROp], Int, IR)
translateBody l idx ir = let (res, i, ir'@IR{irAllocatedStrings=strs}) = aux l idx [] ir in (res, i, ir'{irAllocatedStrings = strs})
  where
    aux :: [L.SExpr] -> Int -> [IROp] -> IR -> ([IROp], Int, IR)
    aux [] i acc irAux = (acc, i, irAux)
    aux (x : xs) i acc irAux = let (op, ni, irAux') = fromSExpr x i irAux in aux xs ni (op : acc) irAux'

tempVarIdent :: Int -> T.Text
tempVarIdent i = T.pack $ "t" <> (show i)

mkIR :: IR
mkIR = IR{irAllocatedStrings=[], irStringIdx=0}

-- main ir functions
fromSExpr :: L.SExpr -> Int -> IR -> (IROp, Int, IR)
fromSExpr (L.SELiteral ty lit _ _) i ir@(IR{..})=
  let 
    (v, ir') = 
      case ty of
        L.Int' -> (IRInt $ read @Int (T.unpack lit), Nothing)
        L.Float' -> (IRFloat $ read @Double (T.unpack lit), Nothing)
        L.Bool' -> (IRBool $ read @Bool (T.unpack lit), Nothing)
        L.Char' -> (IRChar $ read @Char (T.unpack lit), Nothing)
        L.Unit' -> (IRUnit, Nothing)
        L.Undef' -> (IRUndef, Nothing)
        L.String' ->
          let allocStrs = lit : irAllocatedStrings in
          (IRString $ T.show irStringIdx, Just $ ir{irAllocatedStrings= allocStrs, irStringIdx=irStringIdx + 1})
  in
  case ir' of
    Nothing -> (v, i, ir)
    Just x -> (v, i, x)
fromSExpr (L.SEBinary op _ _ l r) i ir =
  let
    (el, ni, ir') = fromSExpr l i ir
    (er, fi, ir'') = fromSExpr r ni ir'
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
  in (IRVar (tempVarIdent $ fi + 1) (irOp el er), fi + 1, ir'')
fromSExpr (L.SEUnary op _ _ v) i ir =
  let
    (ev, ni, ir') = fromSExpr v i ir
    irOp = 
      case op of
        L.Not -> IRUn L.Not
        L.Negate -> IRUn L.Negate
  in (IRVar (tempVarIdent $ ni + 1) (irOp ev), ni + 1, ir')
fromSExpr (L.SEVar _ _ L.Var{varIdent=ident, varValue=val}) i ir = 
  let (evaluated_value, ni, ir') = fromSExpr val i ir in ((IRVar ident evaluated_value), ni, ir')
fromSExpr (L.SEConst _ _ L.Var{varIdent=ident, varValue=val}) i ir = 
  let (evaluated_value, ni, ir') = fromSExpr val i ir in ((IRConst ident evaluated_value), ni, ir')
fromSExpr (L.SEFunc L.Func{funcIdent=ident, funcArgs=args, funcReturnType=ret, funcBody=body}) i ir =
  let (tbody, ni, ir') = translateBody body i ir in (IRFunc ident args tbody ret, ni, ir')
fromSExpr (L.SESet _ _ ident val) i ir = 
  let (evaluated_value, ni, ir') = fromSExpr val i ir in ((IRVar ident evaluated_value), ni, ir')
fromSExpr (L.SEIdentifier ident _ _) i ir = (IRIdent ident, i, ir)
fromSExpr (L.SEReturn _ _ v) i ir = 
  let (ev, ni, ir') = fromSExpr v i ir in (IRRet ev, ni, ir')
fromSExpr (L.SEPrint _ _ v) i ir = 
  let (ev, ni, ir') = fromSExpr v i ir in (IRPrint ev, ni, ir')
fromSExpr (L.SEIfStmt _ _ cond tbranch Nothing) i ir =
  let 
    (econd, ni, ir') = fromSExpr cond i ir
    (etbr, fi, ir'') = fromSExpr tbranch ni ir'
    -- no index, as it's assigned in label application to prevent clashing
    label = "L" 
    gotomain = "blank" -- blank goto because labels haven't been applied to the rest.
  in (IRIf econd gotomain (label, Label (label, [etbr, IRGoto gotomain])) Nothing, fi + 1, ir'')
fromSExpr (L.SEIfStmt _ _ cond tbranch (Just fbranch)) i ir =
  let 
    (econd, ni, ir') = fromSExpr cond i ir
    (etbr, ti, ir'') = fromSExpr tbranch ni ir'
    (efbr, fi, ir''') = fromSExpr fbranch ti ir''
    -- no index, as it's assigned in label application to prevent clashing
    label = "L"
    gotomain = "blank" -- blank goto because labels haven't been applied to the rest.
  in (IRIf econd gotomain (label, Label (label, [etbr, IRGoto gotomain])) (Just (label, Label (label, [efbr, IRGoto gotomain]))), fi + 1, ir''')
fromSExpr (L.SEFuncCall _ _ ident vals) i ir =
  let (eargs, ni, ir') = translateBody vals i ir in
  (IRFuncCall ident eargs, ni, ir')
fromSExpr (L.SEBlockStmt _ _ vals) i ir =
  let (ebody, ni, ir') = translateBody vals i ir in
  (IRBlockStmt $ reverse ebody, ni, ir')

applyLabels :: [IROp] -> Int -> ([IROp], Int)
applyLabels prog i = aux prog (Label ((labelSuffix i), [])) [] i
  where
    labelSuffix :: Int -> T.Text
    labelSuffix index = "L" <> (T.show index)

    aux :: [IROp] -> Label -> [IROp] -> Int -> ([IROp], Int)
    aux [] (Label (n, exprs)) acc idx = 
      if length exprs == 0 then (acc, idx)
                           else ((IRLabel $ Label (n, reverse exprs)) : acc, idx)
    aux ((IRFunc ident args fexprs ret) : rest) old_lbl@(Label (_, exprs)) acc idx =
      let 
        (led_body, next_idx) = applyLabels fexprs idx
        led_func = IRFunc ident args led_body ret 
      in if length exprs == 0 then aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : acc) (next_idx + 1)
         else aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : (IRLabel old_lbl) : acc) (next_idx + 1)

    aux (IRIf cond gotomain (gototrue, (Label (_, exprs))) Nothing : rest) old_lbl@(Label (_, label_exprs)) acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_if = IRIf cond gotomain (labelled_true, (Label (labelled_true, exprs))) Nothing
      in if length label_exprs == 0 then aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : acc) (idx + 2)
         else aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : (IRLabel old_lbl) : acc) (idx + 2)
    aux (IRIf cond gotomain (gototrue, (Label (_, texprs))) (Just (gotofalse, Label (_, fexprs))) : rest) old_lbl@(Label (_, label_exprs)) acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_false = gotofalse <> (T.show $ idx + 2)
        labelled_if = IRIf cond gotomain (labelled_true, (Label (labelled_true, texprs))) (Just (labelled_false, Label (labelled_false, fexprs)))
      in if length label_exprs == 0 then aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : acc) (idx + 3)
         else aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : (IRLabel old_lbl) : acc) (idx + 3)

    aux (goto@(IRGoto _) : rest) old_lbl@(Label (_, exprs)) acc idx = 
      if length exprs == 0 then aux rest (Label (labelSuffix $ idx + 1, [])) (goto : acc) (idx + 1)
                           else aux rest (Label (labelSuffix $ idx + 1, [])) (goto : (IRLabel old_lbl) : acc) (idx + 1)
    aux (expr : rest) curr_lbl acc idx = aux rest (pushExpr curr_lbl expr) acc idx

patchJumps :: NE.NonEmpty IROp -> [IROp]
patchJumps = aux . NE.toList
  where
    aux :: [IROp] -> [IROp]
    aux [] = []
    aux [(IRIf cond _ (gototrue, Label (_, exprs)) Nothing)] =
      let 
        endlbl = IRLabel $ Label ("end", [IRRet IRUnit])
        patched_exprs = (init exprs) ++ [IRGoto "end"] 
      in
      IRIf cond "end" (gototrue, Label (gototrue, patched_exprs)) Nothing : endlbl : []
    aux [(IRIf cond _ (gototrue, Label (_, texprs)) (Just (gotofalse, Label (_, fexprs))))] =
      let 
        endlbl = IRLabel $ Label ("end", [IRRet IRUnit])
        patched_texprs = (init texprs) ++ [IRGoto "end"] 
        patched_fexprs = (init fexprs) ++ [IRGoto "end"] 
      in
      IRIf cond "end" (gototrue, Label (gototrue, patched_texprs)) (Just (gotofalse, Label (gotofalse, patched_fexprs))) : endlbl : []
    aux ((IRIf cond _ (gototrue, Label (_, exprs)) Nothing) : next@(IRLabel (Label (n, _))) : rest) =
      let patched_exprs = (init exprs) ++ [IRGoto n] in
      IRIf cond n (gototrue, Label (gototrue, patched_exprs)) Nothing : next : (aux rest)
    aux ((IRIf cond _ (gototrue, Label (_, texprs)) (Just (gotofalse, Label (_, fexprs)))) : next@(IRLabel (Label (n, _))) : rest) =
      let 
        patched_texprs = (init texprs) ++ [IRGoto n] 
        patched_fexprs = (init fexprs) ++ [IRGoto n] 
      in
      IRIf cond n (gototrue, Label (gototrue, patched_texprs)) (Just (gotofalse, Label (gotofalse, patched_fexprs))) : next : (aux rest) 
    aux (IRLabel (Label (n, exprs)) : rest) = 
      let patched_body = reverse $ aux exprs in IRLabel (Label (n, patched_body)) : aux rest
    aux (IRBlockStmt exprs : rest) = 
      let patched_body = aux exprs in IRBlockStmt patched_body : aux rest
    aux (IRFunc n args exprs ret : rest) = 
      let patched_body = aux exprs in IRFunc n args patched_body ret : aux rest
    aux (expr : rest) = expr : aux rest

programToIR :: L.Program -> ([IROp], IR)
programToIR (L.Program sexprs) = 
  let 
    (conv, _, IR{irAllocatedStrings=strs, irStringIdx=stridx}) = translateBody sexprs 0 mkIR
    (labelled, _) = applyLabels conv 0
    res = patchJumps $ NE.fromList labelled
  in (res, IR{irAllocatedStrings=reverse strs, irStringIdx = stridx})

ppIR :: L.Program -> IO ()
ppIR prog =
  let (progIR, ir) = programToIR prog in
  putDoc $ PP.sep $ (map PP.pretty progIR) <> [border, formatTracker ir]
  where
    border :: PP.Doc T.Text
    border = (PP.pretty @T.Text $ T.pack $ replicate 30 '-') <> PP.line

    formatTracker :: IR -> PP.Doc T.Text
    formatTracker IR{irAllocatedStrings=strs} =
      (PP.pretty @T.Text "DATA:") <> PP.line <> ((PP.indent 2 . PP.vsep) $ formatAllocStrs strs 0) <> PP.line

    formatAllocStrs :: [T.Text] -> Int -> [PP.Doc T.Text]
    formatAllocStrs [] _ = []
    formatAllocStrs (s : rest) i =
      ((PP.pretty @T.Text "string[") <> (PP.viaShow i) <> (PP.pretty @T.Text "] =") PP.<+> (PP.viaShow s)) : formatAllocStrs rest (i + 1)
