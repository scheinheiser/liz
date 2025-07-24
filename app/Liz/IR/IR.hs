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
import Data.List (mapAccumL)

-- helper functions
pushExpr :: Label -> IROp -> Label
pushExpr (Label (n, exprs)) instr = Label (n, instr : exprs)

-- TODO: refactor to use `mapAccumL`
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
fromSExpr (L.SEExpr ex) i at =
  let (ex', i', at') = fromExpr ex i at in
  (IRExpr ex', i', at')
fromSExpr (L.SEIfStmt _ _ cond tbranch Nothing) i at =
  let 
    (cond', i', at') = fromExpr cond i at
    (tbr', i'', at'') = fromSExpr tbranch i' at'
    -- no index, as it's assigned in label application to prevent clashing
    label = "L" 
    gotomain = "blank" -- blank goto because labels haven't been applied to the rest.
  in (IRFlow $ IfStmt cond' gotomain (label, Label (label, [tbr'])) Nothing, i'' + 1, at'')
fromSExpr (L.SEIfStmt _ _ cond tbranch (Just fbranch)) i at =
  let 
    (cond', i', at') = fromExpr cond i at
    (tbr', i'', at'') = fromSExpr tbranch i' at'
    (fbr', i''', at''') = fromSExpr fbranch i'' at''
    label = "L"
    gotomain = "blank"
  in (IRFlow $ IfStmt cond' gotomain (label, Label (label, [tbr'])) (Just (label, Label (label, [fbr']))), i''' + 1, at''')
fromSExpr (L.SEBlockStmt _ _ vals) i at =
  let (ebody, i', at') = translateBody vals i at in
  (IRBlockStmt $ reverse ebody, i', at')
fromSExpr (L.SEFunc L.Func{funcIdent=ident, funcArgs=args, funcReturnType=ret, funcBody=body}) i at =
  let (tbody, i', at') = translateBody body i at in (IRFunc $ Fn ident args tbody ret, i', at')
fromSExpr v _ _ = error (show v)

fromExpr :: L.Expression -> Int -> AllocTracker -> (Expr, Int, AllocTracker)
fromExpr (L.EIdentifier ident _ _) i at = (Ident ident, i, at)
fromExpr (L.EBinary op _ _ l r) i at = 
  let
    (l', i', at') = fromSExpr l i at
    (r', i'', at'') = fromSExpr r i' at'
    (bin, i''') = fromBinary op l' r' i''
  in (bin, i''', at'')
  where
    fromBinary :: L.BinaryOp -> IROp -> IROp -> Int -> (Expr, Int)
    fromBinary op' l'' r'' idx =
      let temp_i = tempVarIdent idx in 
      (Bin temp_i op' l'' r'', idx + 1)
fromExpr (L.EUnary op _ _ v) i at = 
  let 
    (v', i', at') = fromSExpr v i at
    (un, i'') = fromUnary op v' i'
  in (un, i'', at')
  where
    fromUnary :: L.UnaryOp -> IROp -> Int -> (Expr, Int)
    fromUnary op' v'' idx =
      let temp_i = tempVarIdent idx in 
      (Un temp_i op' v'', idx + 1)
fromExpr (L.ELiteral ty lit _ _) i at@(AllocTracker{..}) =
  let 
    (v, at') = 
      case ty of
        L.Int' -> (EVal $ Integ $ read @Int (T.unpack lit), Nothing)
        L.Float' -> (EVal $ Flt $ read @Double (T.unpack lit), Nothing)
        L.Bool' -> (EVal $ Bln $ read @Bool (T.unpack lit), Nothing)
        L.Unit' -> (EVal Unt, Nothing)
        L.String'; L.Char' ->
          let allocStrs = lit : atAllocatedStrings in
          (EVal $ Str $ T.show atStringIdx, Just $ at{atAllocatedStrings= allocStrs, atStringIdx=atStringIdx + 1})
  in
  case at' of
    Nothing -> (v, i, at)
    Just x -> (v, i, x)
fromExpr (L.EVar _ _ L.Var{varIdent = ident, varValue = v}) i at =
  let (v', i', at') = fromSExpr v i at in
  (Var ident v', i', at')
fromExpr (L.ESet _ _ ident v) i at =
  let (v', i', at') = fromSExpr v i at in
  (Var ident v', i', at')
fromExpr (L.EConst _ _ L.Var{varIdent = ident, varValue = v}) i at =
  let (v', i', at') = fromSExpr v i at in
  (Var ident v', i', at')
fromExpr (L.EPrint _ _ v) i at =
  let (v', i', at') = fromSExpr v i at in
  (Print v', i', at')
fromExpr (L.EReturn _ _ v) i at =
  let (v', i', at') = fromSExpr v i at in
  (Ret v', i', at')
fromExpr (L.EFuncCall _ _ ident params) i at =
  let ((at''', i'''), params') = mapAccumL (\(at', i') p -> let (ex, i'', at'') = fromSExpr p i' at' in ((at'', i''), ex)) (at, i) params in
  (FuncCall ident params', i''', at''')

-- FIX: detect if there's nested if statements
applyLabels :: [IROp] -> Int -> ([IROp], Int)
applyLabels prog i = aux prog (Label ((labelSuffix i), [])) [] i
  where
    labelSuffix :: Int -> T.Text
    labelSuffix index = "L" <> (T.show index)

    aux :: [IROp] -> Label -> [IROp] -> Int -> ([IROp], Int)
    aux [] (Label (n, exprs)) acc idx = 
      if length exprs == 0 then (acc, idx)
                           else ((IRFlow $ Lbl $ Label (n, reverse exprs)) : acc, idx)
    aux ((IRFunc (Fn ident args fexprs ret)) : rest) old_lbl@(Label (_, exprs)) acc idx =
      let 
        (led_body, next_idx) = applyLabels fexprs idx
        led_func = IRFunc $ Fn ident args led_body ret 
      in if length exprs == 0 then aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : acc) (next_idx + 1)
         else aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : (IRFlow $ Lbl old_lbl) : acc) (next_idx + 1)

    aux (IRFlow (IfStmt cond gotomain (gototrue, (Label (_, exprs))) Nothing) : rest) old_lbl@(Label (_, label_exprs)) acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_if = IRFlow $ IfStmt cond gotomain (labelled_true, (Label (labelled_true, exprs))) Nothing
      in if length label_exprs == 0 then aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : acc) (idx + 2)
         else aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : (IRFlow $ Lbl old_lbl) : acc) (idx + 2)
    aux (IRFlow (IfStmt cond gotomain (gototrue, (Label (_, texprs))) (Just (gotofalse, Label (_, fexprs)))) : rest) old_lbl@(Label (_, label_exprs)) acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_false = gotofalse <> (T.show $ idx + 2)
        labelled_if = IRFlow $ IfStmt cond gotomain (labelled_true, (Label (labelled_true, texprs))) (Just (labelled_false, Label (labelled_false, fexprs)))
      in if length label_exprs == 0 then aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : acc) (idx + 3)
         else aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : (IRFlow $ Lbl old_lbl) : acc) (idx + 3)

    aux (goto@(IRGoto _) : rest) old_lbl@(Label (_, exprs)) acc idx = 
      if length exprs == 0 then aux rest (Label (labelSuffix $ idx + 1, [])) (goto : acc) (idx + 1)
                           else aux rest (Label (labelSuffix $ idx + 1, [])) (goto : (IRFlow $ Lbl old_lbl) : acc) (idx + 1)
    aux (expr : rest) curr_lbl acc idx = aux rest (pushExpr curr_lbl expr) acc idx

-- FIX: detect if there's nested if statements
patchJumps :: NE.NonEmpty IROp -> [IROp]
patchJumps = aux . NE.toList
  where
    aux :: [IROp] -> [IROp]
    aux [] = []
    aux [(IRFlow (IfStmt cond _ (gototrue, Label (_, exprs)) Nothing))] =
      let 
        lastE = getLast $ last exprs
        endlbl = IRFlow $ Lbl $ Label ("end", [IRExpr $ Phi "res" [(gototrue, lastE)]])
        patched_exprs = exprs <> [IRGoto "end"] 
      in
      IRFlow (IfStmt cond "end" (gototrue, Label (gototrue, patched_exprs)) Nothing) : endlbl : []
    aux [(IRFlow (IfStmt cond _ (gototrue, Label (_, texprs)) (Just (gotofalse, Label (_, fexprs)))))] =
      let 
        (lastT, lastF) = (getLast $ last texprs, getLast $ last fexprs)
        endlbl = IRFlow $ Lbl $ Label ("end", [(IRExpr $ Phi "res" [(gototrue, lastT), (gotofalse, lastF)]), (IRExpr $ Ret (IRExpr $ Ident "res"))])
        patched_texprs = texprs <> [IRGoto "end"] 
        patched_fexprs = fexprs <> [IRGoto "end"] 
      in
      IRFlow (IfStmt cond "end" (gototrue, Label (gototrue, patched_texprs)) (Just (gotofalse, Label (gotofalse, patched_fexprs)))) : endlbl : []
    aux ((IRFlow (IfStmt cond _ (gototrue, Label (_, exprs)) Nothing)) : next@(IRFlow (Lbl (Label (n, _)))) : rest) =
      let patched_exprs = exprs <> [IRGoto n] in
      IRFlow (IfStmt cond n (gototrue, Label (gototrue, patched_exprs)) Nothing) : next : (aux rest)
    aux ((IRFlow (IfStmt cond _ (gototrue, Label (_, texprs)) (Just (gotofalse, Label (_, fexprs))))) : next@(IRFlow (Lbl ((Label (n, _))))) : rest) =
      let 
        patched_texprs = texprs <> [IRGoto n] 
        patched_fexprs = fexprs <> [IRGoto n] 
      in
      IRFlow (IfStmt cond n (gototrue, Label (gototrue, patched_texprs)) (Just (gotofalse, Label (gotofalse, patched_fexprs)))) : next : (aux rest) 
    aux (IRFlow (Lbl (Label (n, exprs))) : rest) = 
      let patched_body = reverse $ aux exprs in IRFlow (Lbl (Label (n, patched_body))) : aux rest
    aux (IRBlockStmt exprs : rest) = 
      let patched_body = aux exprs in IRBlockStmt patched_body : aux rest
    aux (IRFunc (Fn n args exprs ret) : rest) = 
      let patched_body = aux exprs in (IRFunc $ Fn n args patched_body ret) : aux rest
    aux (expr : rest) = expr : aux rest

    getLast :: IROp -> Expr
    getLast (IRBlockStmt b) = getLast $ last b
    getLast (IRExpr op@((Bin _ _ _ _);(Un _ _ _))) = op
    getLast (IRExpr op@(FuncCall _ _)) = op
    getLast (IRExpr (Var i _)) = Ident i
    getLast (IRExpr ret@(Ret _)) = ret
    getLast (IRExpr (Print _)) = EVal Unt
    getLast (IRFlow (IfStmt _ _ (_, Label (_, b)) Nothing)) = getLast $ last b
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
