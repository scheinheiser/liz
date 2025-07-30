{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OrPatterns #-}

module Liz.IR.IR (ppIR, programToIR) where

import Liz.IR.IRTypes
import qualified Liz.Common.Types as L

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.List (mapAccumL)

import qualified Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)

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
mkTracker = AllocTracker{atAllocatedStrings=[], atStringIdx=0, atSymbols=M.empty}

-- main ir functions
fromSExpr :: L.SExpr -> Int -> AllocTracker -> (IROp, Int, AllocTracker)
fromSExpr (L.SEExpr ex) i at =
  let (ex', i', at') = fromExpr ex i at in
  (IRExpr ex', i', at')
fromSExpr (L.SEFlow flow) i at =
  let (ex', i', at') = fromCFlow flow i at in
  (IRFlow ex', i', at')
fromSExpr (L.SEVar _ L.Var{varIdent = ident, varType = t, varValue = v}) i at =
  let 
    (v', i', at'@AllocTracker{atSymbols=symmap}) = fromExpr v i at 
    symmap' = ident `M.insert` t $ symmap
  in
  (Var ident t v', i', at'{atSymbols=symmap'})
fromSExpr (L.SESet _ ident v) i at@AllocTracker{atSymbols=symmap} =
  let (v', i', at') = fromExpr v i at in
  (Var ident (symmap M.! ident) v', i', at')
fromSExpr (L.SEConst _ L.Var{varIdent = ident, varType = t, varValue = v}) i at =
  let 
    (v', i', at'@AllocTracker{atSymbols=symmap}) = fromExpr v i at 
    symmap' = ident `M.insert` t $ symmap
  in
  (Var ident t v', i', at'{atSymbols=symmap'})
fromSExpr v _ _ = error (show v)

fromCFlow :: L.ControlFlow -> Int -> AllocTracker -> (CFlow, Int, AllocTracker)
fromCFlow (L.FIfStmt _ cond tbranch Nothing) i at =
  let 
    (cond', i', at') = fromExpr cond i at
    (tbr', i'', at'') = fromSExpr tbranch i' at'
    -- no index, as it's assigned in label application to prevent clashing
    label = "L" 
    gotomain = "blank" -- blank goto because labels haven't been applied to the rest.
  in (IfStmt cond' gotomain (Label (label, [tbr'])) Nothing, i'' + 1, at'')
fromCFlow (L.FIfStmt _ cond tbranch (Just fbranch)) i at =
  let 
    (cond', i', at') = fromExpr cond i at
    (tbr', i'', at'') = fromSExpr tbranch i' at'
    (fbr', i''', at''') = fromSExpr fbranch i'' at''
    label = "L"
    gotomain = "blank"
  in (IfStmt cond' gotomain (Label (label, [tbr'])) (Just (Label (label, [fbr']))), i''' + 1, at''')
fromCFlow (L.FBlockStmt _ vals) i at =
  let (ebody, i', at') = translateBody vals i at in
  (BlockStmt $ reverse ebody, i', at')
fromCFlow (L.FFunc L.Func{funcIdent=ident, funcArgs=args, funcReturnType=ret, funcBody=body}) i at =
  let 
    (tbody, i', at'@AllocTracker{atSymbols=symmap}) = translateBody body i at 
    symmap' = ident `M.insert` ret $ symmap
  in 
  (Fn ident args (reverse tbody) ret, i', at'{atSymbols=symmap'})

getType :: M.Map T.Text L.Type -> Expr -> L.Type
getType _ (EVal (Integ _)) = L.Int'
getType _ (EVal (Flt _)) = L.Float'
getType _ (EVal (Bln _)) = L.Bool'
getType _ (EVal (Str _)) = L.String'
getType _ (EVal Unt) = L.Unit'
getType m (Bin _ _ l _) = getType m l
getType m (Un _ _ v) = getType m v
getType _ (Print _) = L.Unit'
getType m (Ret v) = getType m v
getType m (Ident i) = m M.! i
getType m (FuncCall i _) = m M.! i
getType _ e = error $ "called getType with unexpected expr: " <> (show e)

fromExpr :: L.Expression -> Int -> AllocTracker -> (Expr, Int, AllocTracker)
fromExpr (L.EIdentifier ident _) i at = (Ident ident, i, at)
fromExpr (L.EBinary op _ l r) i at = 
  let
    (l', i', at') = fromExpr l i at
    (r', i'', at''@(AllocTracker{atSymbols=symmap})) = fromExpr r i' at'
    (bin, i''', symmap') = fromBinary op l' r' i'' symmap
  in (bin, i''', at''{atSymbols = symmap'})
  where
    fromBinary :: L.BinaryOp -> Expr -> Expr -> Int -> M.Map T.Text L.Type -> (Expr, Int, M.Map T.Text L.Type)
    fromBinary op' l'' r'' idx smap =
      let 
        temp_i = tempVarIdent idx 
        -- each operand is guaranteed to have the same type, so using just the left is fine.
        smap' = temp_i `M.insert` (getType smap l'') $ smap
      in 
      (Bin temp_i op' l'' r'', idx + 1, smap')
fromExpr (L.EUnary op _ v) i at = 
  let 
    (v', i', at'@(AllocTracker{atSymbols=symmap})) = fromExpr v i at
    (un, i'', symmap') = fromUnary op v' i' symmap
  in (un, i'', at'{atSymbols=symmap'})
  where
    fromUnary :: L.UnaryOp -> Expr -> Int -> M.Map T.Text L.Type -> (Expr, Int, M.Map T.Text L.Type)
    fromUnary op' v'' idx smap =
      let 
        temp_i = tempVarIdent idx 
        smap' = temp_i `M.insert` (getType smap v'') $ smap
      in 
      (Un temp_i op' v'', idx + 1, smap')
fromExpr (L.ELiteral ty lit _) i at@(AllocTracker{..}) =
  let 
    (v, at') = 
      case ty of
        L.Int' -> (EVal $ Integ $ read @Int (T.unpack lit), Nothing)
        L.Float' -> (EVal $ Flt $ read @Double (T.unpack lit), Nothing)
        L.Bool' -> (EVal $ Bln $ read @Bool (T.unpack lit), Nothing)
        L.Unit' -> (EVal Unt, Nothing)
        L.String'; L.Char' ->
          let allocStrs = lit : atAllocatedStrings in
          (EVal $ Str $ T.show atStringIdx, Just $ at{atAllocatedStrings= allocStrs, atStringIdx=atStringIdx + 1, atSymbols=atSymbols})
  in
  case at' of
    Nothing -> (v, i, at)
    Just x -> (v, i, x)
fromExpr (L.EPrint _ v) i at =
  let (v', i', at') = fromExpr v i at in
  (Print v', i', at')
fromExpr (L.EReturn _ v) i at =
  let (v', i', at') = fromExpr v i at in
  (Ret v', i', at')
fromExpr (L.EFuncCall _ ident params) i at =
  let ((at''', i'''), params') = mapAccumL (\(at', i') p -> let (ex, i'', at'') = fromExpr p i' at' in ((at'', i''), ex)) (at, i) params in
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
    aux ((IRFlow (Fn ident args fexprs ret)) : rest) old_lbl@(Label (_, exprs)) acc idx =
      let 
        (led_body, next_idx) = applyLabels fexprs idx
        led_func = IRFlow $ Fn ident args led_body ret 
      in if length exprs == 0 then aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : acc) (next_idx + 1)
         else aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : (IRFlow $ Lbl old_lbl) : acc) (next_idx + 1)

    aux (IRFlow (IfStmt cond gotomain (Label (gototrue, exprs)) Nothing) : rest) old_lbl@(Label (_, label_exprs)) acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_if = IRFlow $ IfStmt cond gotomain (Label (labelled_true, exprs)) Nothing
      in if length label_exprs == 0 then aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : acc) (idx + 2)
         else aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : (IRFlow $ Lbl old_lbl) : acc) (idx + 2)
    aux (IRFlow (IfStmt cond gotomain (Label (gototrue, texprs)) (Just (Label (gotofalse, fexprs)))) : rest) old_lbl@(Label (_, label_exprs)) acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_false = gotofalse <> (T.show $ idx + 2)
        labelled_if = IRFlow $ IfStmt cond gotomain (Label (labelled_true, texprs)) (Just (Label (labelled_false, fexprs)))
      in if length label_exprs == 0 then aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : acc) (idx + 3)
         else aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : (IRFlow $ Lbl old_lbl) : acc) (idx + 3)

    aux (goto@(IRFlow (CGoto _)) : rest) old_lbl@(Label (_, exprs)) acc idx = 
      if length exprs == 0 then aux rest (Label (labelSuffix $ idx + 1, [])) (goto : acc) (idx + 1)
                           else aux rest (Label (labelSuffix $ idx + 1, [])) (goto : (IRFlow $ Lbl old_lbl) : acc) (idx + 1)
    aux (expr : rest) curr_lbl acc idx = aux rest (pushExpr curr_lbl expr) acc idx

-- FIX: detect if there's nested if statements
patchJumps :: NE.NonEmpty IROp -> [IROp]
patchJumps = aux . NE.toList
  where
    aux :: [IROp] -> [IROp]
    aux [] = []
    aux [(IRFlow (IfStmt cond _ (Label (gototrue, exprs)) Nothing))] =
      let 
        lastE = getLast $ last exprs
        endlbl = IRFlow $ Lbl $ Label ("end", [IRExpr $ Phi "res" [(gototrue, lastE)]])
        patched_exprs = exprs <> [IRFlow $ CGoto "end"] 
      in
      IRFlow (IfStmt cond "end" (Label (gototrue, patched_exprs)) Nothing) : endlbl : []
    aux [(IRFlow (IfStmt cond _ (Label (gototrue, texprs)) (Just (Label (gotofalse, fexprs)))))] =
      let 
        (lastT, lastF) = (getLast $ last texprs, getLast $ last fexprs)
        endlbl = IRFlow $ Lbl $ Label ("end", [(IRExpr $ Phi "res" [(gototrue, lastT), (gotofalse, lastF)]), (IRExpr $ Ret (Ident "res"))])
        patched_texprs = texprs <> [IRFlow $ CGoto "end"] 
        patched_fexprs = fexprs <> [IRFlow $ CGoto "end"] 
      in
      IRFlow (IfStmt cond "end" (Label (gototrue, patched_texprs)) (Just (Label (gotofalse, patched_fexprs)))) : endlbl : []
    aux ((IRFlow (IfStmt cond _ (Label (gototrue, exprs)) Nothing)) : next@(IRFlow (Lbl (Label (n, _)))) : rest) =
      let patched_exprs = exprs <> [IRFlow $ CGoto n] in
      IRFlow (IfStmt cond n (Label (gototrue, patched_exprs)) Nothing) : next : (aux rest)
    aux ((IRFlow (IfStmt cond _ (Label (gototrue, texprs)) (Just (Label (gotofalse, fexprs))))) : next@(IRFlow (Lbl ((Label (n, _))))) : rest) =
      let 
        patched_texprs = texprs <> [IRFlow $ CGoto n] 
        patched_fexprs = fexprs <> [IRFlow $ CGoto n] 
      in
      IRFlow (IfStmt cond n (Label (gototrue, patched_texprs)) (Just (Label (gotofalse, patched_fexprs)))) : next : (aux rest) 
    aux (IRFlow (Lbl (Label (n, exprs))) : rest) = 
      let patched_body = reverse $ aux exprs in IRFlow (Lbl (Label (n, patched_body))) : aux rest
    aux (IRFlow (BlockStmt exprs) : rest) = 
      let patched_body = aux exprs in IRFlow (BlockStmt patched_body) : aux rest
    aux (IRFlow (Fn n args exprs ret) : rest) = 
      let patched_body = aux exprs in (IRFlow $ Fn n args patched_body ret) : aux rest
    aux (expr : rest) = expr : aux rest

    getLast :: IROp -> Expr
    getLast (IRFlow (BlockStmt b)) = getLast $ last b
    getLast (IRExpr (Bin i _ _ _)) = Ident i
    getLast (IRExpr (Un i _ _)) = Ident i
    getLast (IRExpr op@(FuncCall _ _)) = op
    getLast (Var i _ _) = Ident i
    getLast (IRExpr ret@(Ret _)) = ret
    getLast (IRExpr (Print _)) = EVal Unt
    getLast (IRFlow (IfStmt _ _ (Label (_, b)) Nothing)) = getLast $ last b
    -- TODO: do something for this case:
    -- getLast (IRIF _ (Label (_, bt)) (Just (Label (_, bf)))) = [getLast bt, getLast bf]
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
