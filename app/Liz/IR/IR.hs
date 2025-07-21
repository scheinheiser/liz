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

translateBody :: [L.SExpr] -> Int -> AllocTracker -> ([IROp], Int, AllocTracker)
translateBody l idx at = let (res, i, at'@AllocTracker{atAllocatedStrings=strs}) = aux l idx [] at in (res, i, at'{atAllocatedStrings = strs})
  where
    aux :: [L.SExpr] -> Int -> [IROp] -> AllocTracker -> ([IROp], Int, AllocTracker)
    aux [] i acc atAux = (acc, i, atAux)
    aux (x : xs) i acc atAux = let (op, i', atAux') = fromSExpr x i atAux in aux xs i' (op : acc) atAux'

tempVarIdent :: Int -> T.Text
tempVarIdent i = T.pack $ "t" <> (show i)

mkTracker :: AllocTracker
mkTracker = AllocTracker{atAllocatedStrings=[], atStringIdx=0}

-- main ir functions
fromSExpr :: L.SExpr -> Int -> AllocTracker -> (IROp, Int, AllocTracker)
fromSExpr (L.SELiteral ty lit _ _) i at@(AllocTracker{..})=
  let 
    (v, at') = 
      case ty of
        L.Int' -> (IRValue $ Integ $ read @Int (T.unpack lit), Nothing)
        L.Float' -> (IRValue $ Flt $ read @Double (T.unpack lit), Nothing)
        L.Bool' -> (IRValue $ Bln $ read @Bool (T.unpack lit), Nothing)
        L.Unit' -> (IRValue Unt, Nothing)
        L.String'; L.Char' ->
          let allocStrs = lit : atAllocatedStrings in
          (IRValue $ Str $ T.show atStringIdx, Just $ at{atAllocatedStrings= allocStrs, atStringIdx=atStringIdx + 1})
  in
  case at' of
    Nothing -> (v, i, at)
    Just x -> (v, i, x)
fromSExpr (L.SEBinary op _ _ l r) i at =
  let
    (el, i', at') = fromSExpr l i at
    (er, i'', at'') = fromSExpr r i' at'
    irOp = 
      case op of
        L.Add -> IRBin (tempVarIdent $ i'' + 1) L.Add
        L.Subtract -> IRBin (tempVarIdent $ i'' + 1) L.Subtract
        L.Multiply -> IRBin (tempVarIdent $ i'' + 1) L.Multiply
        L.Divide -> IRBin (tempVarIdent $ i'' + 1) L.Divide
        L.Concat -> IRBin (tempVarIdent $ i'' + 1) L.Concat
        L.GreaterEql -> IRBin (tempVarIdent $ i'' + 1) L.GreaterEql
        L.LessEql -> IRBin (tempVarIdent $ i'' + 1) L.LessEql
        L.Eql -> IRBin (tempVarIdent $ i'' + 1) L.Eql
        L.NotEql -> IRBin (tempVarIdent $ i'' + 1) L.NotEql
        L.Greater -> IRBin (tempVarIdent $ i'' + 1) L.Greater
        L.Less -> IRBin (tempVarIdent $ i'' + 1) L.Less
  in (irOp el er, i'' + 1, at'')
fromSExpr (L.SEUnary op _ _ v) i at =
  let
    (ev, i', at') = fromSExpr v i at
    irOp = 
      case op of
        L.Not -> IRUn (tempVarIdent $ i' + 1) L.Not
        L.Negate -> IRUn (tempVarIdent $ i' + 1) L.Negate
  in (irOp ev, i' + 1, at')
fromSExpr (L.SEVar _ _ L.Var{varIdent=ident, varValue=val}) i at = 
  let (evaluated_value, i', at') = fromSExpr val i at in ((IRVar ident evaluated_value), i', at')
fromSExpr (L.SEConst _ _ L.Var{varIdent=ident, varValue=val}) i at = 
  let (evaluated_value, i', at') = fromSExpr val i at in ((IRConst ident evaluated_value), i', at')
fromSExpr (L.SEFunc L.Func{funcIdent=ident, funcArgs=args, funcReturnType=ret, funcBody=body}) i at =
  let (tbody, i', at') = translateBody body i at in (IRFunc $ Fn ident args tbody ret, i', at')
fromSExpr (L.SESet _ _ ident val) i at = 
  let (evaluated_value, i', at') = fromSExpr val i at in ((IRVar ident evaluated_value), i', at')
fromSExpr (L.SEIdentifier ident _ _) i at = (IRIdent ident, i, at)
fromSExpr (L.SEReturn _ _ v) i at = 
  let (ev, i', at') = fromSExpr v i at in (IRRet ev, i', at')
fromSExpr (L.SEPrint _ _ v) i at = 
  let (ev, i', at') = fromSExpr v i at in (IRPrint ev, i', at')
fromSExpr (L.SEIfStmt _ _ cond tbranch Nothing) i at =
  let 
    (econd, i', at') = fromSExpr cond i at
    (etbr, i'', at'') = fromSExpr tbranch i' at'
    -- no index, as it's assigned in label application to prevent clashing
    label = "L" 
    gotomain = "blank" -- blank goto because labels haven't been applied to the rest.
  in (IRIf $ IfSt econd gotomain (label, Label (label, [etbr])) Nothing, i'' + 1, at'')
fromSExpr (L.SEIfStmt _ _ cond tbranch (Just fbranch)) i at =
  let 
    (econd, i', at') = fromSExpr cond i at
    (etbr, i'', at'') = fromSExpr tbranch i' at'
    (efbr, i''', at''') = fromSExpr fbranch i'' at''
    label = "L"
    gotomain = "blank"
  in (IRIf $ IfSt econd gotomain (label, Label (label, [etbr])) (Just (label, Label (label, [efbr]))), i''' + 1, at''')
fromSExpr (L.SEFuncCall _ _ ident vals) i at =
  let (eargs, i', at') = translateBody vals i at in
  (IRFuncCall ident eargs, i', at')
fromSExpr (L.SEBlockStmt _ _ vals) i at =
  let (ebody, i', at') = translateBody vals i at in
  (IRBlockStmt $ reverse ebody, i', at')
fromSExpr v _ _ = error (show v)

-- FIX: detect if there's nested if statements
applyLabels :: [IROp] -> Int -> ([IROp], Int)
applyLabels prog i = aux prog (Label ((labelSuffix i), [])) [] i
  where
    labelSuffix :: Int -> T.Text
    labelSuffix index = "L" <> (T.show index)

    aux :: [IROp] -> Label -> [IROp] -> Int -> ([IROp], Int)
    aux [] (Label (n, exprs)) acc idx = 
      if length exprs == 0 then (acc, idx)
                           else ((IRLabel $ Label (n, reverse exprs)) : acc, idx)
    aux ((IRFunc (Fn ident args fexprs ret)) : rest) old_lbl@(Label (_, exprs)) acc idx =
      let 
        (led_body, next_idx) = applyLabels fexprs idx
        led_func = IRFunc $ Fn ident args led_body ret 
      in if length exprs == 0 then aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : acc) (next_idx + 1)
         else aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : (IRLabel old_lbl) : acc) (next_idx + 1)

    aux (IRIf (IfSt cond gotomain (gototrue, (Label (_, exprs))) Nothing) : rest) old_lbl@(Label (_, label_exprs)) acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_if = IRIf $ IfSt cond gotomain (labelled_true, (Label (labelled_true, exprs))) Nothing
      in if length label_exprs == 0 then aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : acc) (idx + 2)
         else aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : (IRLabel old_lbl) : acc) (idx + 2)
    aux (IRIf (IfSt cond gotomain (gototrue, (Label (_, texprs))) (Just (gotofalse, Label (_, fexprs)))) : rest) old_lbl@(Label (_, label_exprs)) acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_false = gotofalse <> (T.show $ idx + 2)
        labelled_if = IRIf $ IfSt cond gotomain (labelled_true, (Label (labelled_true, texprs))) (Just (labelled_false, Label (labelled_false, fexprs)))
      in if length label_exprs == 0 then aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : acc) (idx + 3)
         else aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : (IRLabel old_lbl) : acc) (idx + 3)

    aux (goto@(IRGoto _) : rest) old_lbl@(Label (_, exprs)) acc idx = 
      if length exprs == 0 then aux rest (Label (labelSuffix $ idx + 1, [])) (goto : acc) (idx + 1)
                           else aux rest (Label (labelSuffix $ idx + 1, [])) (goto : (IRLabel old_lbl) : acc) (idx + 1)
    aux (expr : rest) curr_lbl acc idx = aux rest (pushExpr curr_lbl expr) acc idx

-- FIX: detect if there's nested if statements
patchJumps :: NE.NonEmpty IROp -> [IROp]
patchJumps = aux . NE.toList
  where
    aux :: [IROp] -> [IROp]
    aux [] = []
    aux [(IRIf (IfSt cond _ (gototrue, Label (_, exprs)) Nothing))] =
      let 
        lastE = getLast $ last exprs
        endlbl = IRLabel $ Label ("end", [IRPhi "res" [(gototrue, lastE)]])
        patched_exprs = exprs <> [IRGoto "end"] 
      in
      IRIf (IfSt cond "end" (gototrue, Label (gototrue, patched_exprs)) Nothing) : endlbl : []
    aux [(IRIf (IfSt cond _ (gototrue, Label (_, texprs)) (Just (gotofalse, Label (_, fexprs)))))] =
      let 
        (lastT, lastF) = (getLast $ last texprs, getLast $ last fexprs)
        endlbl = IRLabel $ Label ("end", [(IRPhi "res" [(gototrue, lastT), (gotofalse, lastF)]), (IRRet $ IRIdent "res")])
        patched_texprs = texprs <> [IRGoto "end"] 
        patched_fexprs = fexprs <> [IRGoto "end"] 
      in
      IRIf (IfSt cond "end" (gototrue, Label (gototrue, patched_texprs)) (Just (gotofalse, Label (gotofalse, patched_fexprs)))) : endlbl : []
    aux ((IRIf (IfSt cond _ (gototrue, Label (_, exprs)) Nothing)) : next@(IRLabel (Label (n, _))) : rest) =
      let patched_exprs = exprs <> [IRGoto n] in
      IRIf (IfSt cond n (gototrue, Label (gototrue, patched_exprs)) Nothing) : next : (aux rest)
    aux ((IRIf (IfSt cond _ (gototrue, Label (_, texprs)) (Just (gotofalse, Label (_, fexprs))))) : next@(IRLabel (Label (n, _))) : rest) =
      let 
        patched_texprs = texprs <> [IRGoto n] 
        patched_fexprs = fexprs <> [IRGoto n] 
      in
      IRIf (IfSt cond n (gototrue, Label (gototrue, patched_texprs)) (Just (gotofalse, Label (gotofalse, patched_fexprs)))) : next : (aux rest) 
    aux (IRLabel (Label (n, exprs)) : rest) = 
      let patched_body = reverse $ aux exprs in IRLabel (Label (n, patched_body)) : aux rest
    aux (IRBlockStmt exprs : rest) = 
      let patched_body = aux exprs in IRBlockStmt patched_body : aux rest
    aux (IRFunc (Fn n args exprs ret) : rest) = 
      let patched_body = aux exprs in (IRFunc $ Fn n args patched_body ret) : aux rest
    aux (expr : rest) = expr : aux rest

    getLast :: IROp -> IROp
    getLast (IRBlockStmt b) = getLast $ last b
    getLast op@((IRBin _ _ _ _);(IRUn _ _ _)) = op
    getLast op@(IRFuncCall _ _) = op
    getLast (IRVar i _) = IRIdent i
    getLast (IRConst i _) = IRIdent i
    getLast (IRRet v) = v
    getLast (IRPrint _) = IRValue Unt
    getLast (IRIf (IfSt _ _ (_, Label (_, b)) Nothing)) = getLast $ last b
    -- TODO: do something for this case:
    -- getLast (IRIF _ _ (_, Label (_, bt)) (Just (_, Label (_, bf)))) = [getLast bt, getLast bf]
    getLast v = error $ "\nMalformed IR -\n  " <> (show v)

programToIR :: L.Program -> ([IROp], AllocTracker)
programToIR (L.Program sexprs) = 
  let 
    (conv, _, at@AllocTracker{atAllocatedStrings=strs}) = translateBody sexprs 0 mkTracker
    (labelled, _) = applyLabels conv 0
    res = patchJumps $ NE.fromList labelled
  in (res, at{atAllocatedStrings= reverse strs})

ppIR :: L.Program -> IO ()
ppIR prog =
  let (progIR, ir) = programToIR prog in
  putDoc $ PP.sep $ (map PP.pretty progIR) <> [border, formatTracker ir]
  where
    border :: PP.Doc T.Text
    border = (PP.pretty @T.Text $ T.pack $ replicate 30 '-') <> PP.line

    formatTracker :: AllocTracker -> PP.Doc T.Text
    formatTracker AllocTracker{atAllocatedStrings=strs} =
      let 
        contents =
          if length strs == 0 then [(PP.pretty @T.Text "<empty>")]
                              else formatAllocStrs strs 0
      in (PP.pretty @T.Text "DATA:") <> PP.line <> ((PP.indent 2 . PP.vsep) contents) <> PP.line

    formatAllocStrs :: [T.Text] -> Int -> [PP.Doc T.Text]
    formatAllocStrs [] _ = []
    formatAllocStrs (s : rest) i =
      ((PP.pretty @T.Text "string[") <> (PP.viaShow i) <> (PP.pretty @T.Text "] =") PP.<+> (PP.viaShow s)) : formatAllocStrs rest (i + 1)
