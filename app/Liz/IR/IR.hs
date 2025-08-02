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

translateBody :: [L.SExpr] -> IR -> ([IROp], IR)
translateBody l ir =
  let 
    (ir''', res) = 
      mapAccumL 
        (\ir' expr -> 
          let (op, ir'') = fromSExpr expr ir' in 
          (ir'', op)) ir l 
  in
  (res, ir''')

tempVarIdent :: Int -> T.Text
tempVarIdent i = T.pack $ "t" <> (show i)

mkIR :: IR
mkIR = IR [] [] [] 0 0 0 M.empty

getType :: M.Map T.Text L.Type -> Expr -> L.Type
getType _ (EVal (Integ _)) = L.Int'
getType _ (EVal (Flt _)) = L.Float'
getType _ (EVal (Bln _)) = L.Bool'
getType _ (EVal (Str _)) = L.String'
getType _ (EVal Unt) = L.Unit'
getType m (Bin _ _ op l _) = 
  case op of
    L.Less;L.Greater;L.GreaterEql;L.LessEql;L.Eql;L.NotEql -> L.Bool'
    _ -> getType m l
getType m (Un _ _ op v) = 
  case op of
    L.Not -> L.Bool'
    _ -> getType m v
getType _ (Print _) = L.Unit'
getType m (Ret v) = getType m v
getType m (Ident i) = m M.! i
getType m (FuncCall _ _ i _) = m M.! i
getType _ e = error $ "called getType with unexpected expr: " <> (show e)

-- main ir functions
fromSExpr :: L.SExpr -> IR -> (IROp, IR)
fromSExpr (L.SEExpr ex) ir =
  let (ex', ir') = fromExpr ex ir in
  (IRExpr ex', ir')
fromSExpr (L.SEFlow flow) ir =
  let (ex', ir') = fromCFlow flow ir in
  (IRFlow ex', ir')
fromSExpr (L.SEVar _ L.Var{varIdent = ident, varType = t, varValue = v}) ir =
  let 
    (v', ir'@IR{irSymbols=symmap}) = fromExpr v ir 
    symmap' = ident `M.insert` t $ symmap
  in
  (IRVar $ Variable ident t v', ir'{irSymbols=symmap'})
fromSExpr (L.SESet _ ident v) ir@IR{irSymbols=symmap} =
  let (v', ir') = fromExpr v ir in
  (IRVar $ Variable ident (symmap M.! ident) v', ir')
fromSExpr (L.SEConst _ L.Var{varIdent = ident, varType = t, varValue = v}) ir@IR{irSymbols=symmap} =
  let 
    (v', ir') = fromExpr v ir
    symmap' = ident `M.insert` t $ symmap
  in
  (IRVar $ Variable ident t v', ir'{irSymbols=symmap'})

fromCFlow :: L.ControlFlow -> IR -> (CFlow, IR)
fromCFlow (L.FIfStmt _ cond tbranch Nothing) ir@IR{irCFlowIdx=fi} =
  let 
    flowidx = "IFSTMT" <> (T.show fi)
    (cond', ir') = fromExpr cond ir
    (tbr', ir'') = fromSExpr tbranch ir'
    -- no index, as it's assigned in label application to prevent clashing
    label = "L" 
    gotomain = "blank" -- blank goto because labels haven't been applied to the rest.
  in (IfStmt flowidx cond' gotomain (Label (label, [tbr'])) Nothing, ir''{irCFlowIdx=fi + 1})
fromCFlow (L.FIfStmt _ cond tbranch (Just fbranch)) ir@IR{irCFlowIdx=fi} =
  let 
    flowidx = "IFSTMT" <> (T.show fi)
    (cond', ir') = fromExpr cond ir
    (tbr', ir'') = fromSExpr tbranch ir'
    (fbr', ir''') = fromSExpr fbranch ir'' 
    label = "L"
    gotomain = "blank"
  in (IfStmt flowidx cond' gotomain (Label (label, [tbr'])) (Just (Label (label, [fbr']))), ir'''{irCFlowIdx=fi + 1})
fromCFlow (L.FBlockStmt _ vals) ir@IR{irCFlowIdx=fi} =
  let 
    flowidx = "BLOCK" <> (T.show fi)
    (body, ir') = translateBody vals ir 
  in
  (BlockStmt flowidx body, ir'{irCFlowIdx=fi + 1})

fromExpr :: L.Expression -> IR -> (Expr, IR)
fromExpr (L.EIdentifier ident _) ir = (Ident ident, ir)
fromExpr (L.EBinary op _ l r) ir = 
  let
    (l', ir') = fromExpr l ir
    (r', ir''@IR{irTempVarIdx=i, irSymbols=symmap}) = fromExpr r ir'
    (bin, i'', symmap') = fromBinary op l' r' i symmap
  in (bin, ir''{irTempVarIdx=i'', irSymbols=symmap'})
  where
    fromBinary :: L.BinaryOp -> Expr -> Expr -> Int -> M.Map T.Text L.Type -> (Expr, Int, M.Map T.Text L.Type)
    fromBinary op' l'' r'' idx smap =
      let 
        temp_i = tempVarIdent idx 
        -- each operand is guaranteed to have the same type, so using just the left is fine.
        ty = 
          case op' of
            L.Less;L.Greater;L.GreaterEql;L.LessEql;L.Eql;L.NotEql -> L.Bool'
            _ -> getType smap l'' 
        smap' = temp_i `M.insert` ty $ smap
      in 
      (Bin temp_i ty op' l'' r'', idx + 1, smap')
fromExpr (L.EUnary op _ v) ir = 
  let 
    (v', ir'@IR{irTempVarIdx=i, irSymbols=symmap}) = fromExpr v ir
    (un, i', symmap') = fromUnary op v' i symmap
  in (un, ir'{irTempVarIdx=i', irSymbols=symmap'})
  where
    fromUnary :: L.UnaryOp -> Expr -> Int -> M.Map T.Text L.Type -> (Expr, Int, M.Map T.Text L.Type)
    fromUnary op' v'' idx smap =
      let 
        temp_i = tempVarIdent idx 
        ty = 
          case op' of
            L.Not -> L.Bool'
            _ -> getType smap v''
        smap' = temp_i `M.insert` ty $ smap
      in 
      (Un temp_i ty op' v'', idx + 1, smap')
fromExpr (L.ELiteral ty lit _) ir@IR{irAllocatedStrings=strs, irStringIdx=i} =
  let 
    (v, ir') = 
      case ty of
        L.Int' -> (EVal $ Integ $ read @Int (T.unpack lit), Nothing)
        L.Float' -> (EVal $ Flt $ read @Double (T.unpack lit), Nothing)
        L.Bool' -> (EVal $ Bln $ read @Bool (T.unpack lit), Nothing)
        L.Unit' -> (EVal Unt, Nothing)
        L.String'; L.Char' ->
          let allocStrs = (lit, i) : strs in
          (EVal $ Str $ T.show i, Just $ ir{irAllocatedStrings=allocStrs, irStringIdx=i + 1})
  in
  case ir' of
    Nothing -> (v, ir)
    Just x -> (v, x)
fromExpr (L.EPrint _ v) ir = 
  let (v', ir') = fromExpr v ir in
  (Print v', ir')
fromExpr (L.EReturn _ v) ir =
  let (v', ir') = fromExpr v ir in
  (Ret v', ir') 
fromExpr (L.EFuncCall _ ident params) ir@IR{irSymbols=symmap, irTempVarIdx=i} =
  let 
    ((ir''', params'), _) = 
      mapAccumL 
        (\(ir', acc) p -> 
          let (ex, ir'') = fromExpr p ir' in 
          ((ir'', ex : acc), p)) (ir, []) params 
    temp_i = tempVarIdent i
    ty = getType symmap (Ident ident)
  in
  (FuncCall temp_i ty ident params', ir'''{irTempVarIdx=i + 1})

-- FIX: detect if there's nested if statements
applyLabels :: [IROp] -> IR -> ([IROp], IR)
applyLabels prog ir'@IR{irCFlowIdx=i} = aux prog (Label ((labelSuffix i), [])) [] ir' 
  where
    labelSuffix :: Int -> T.Text
    labelSuffix index = "L" <> (T.show index)

    aux :: [IROp] -> Label -> [IROp] -> IR -> ([IROp], IR)
    aux [] (Label (n, exprs)) acc ir = 
      if length exprs == 0 then (acc, ir)
                           else ((IRFlow $ Lbl $ Label (n, reverse exprs)) : acc, ir)

    aux (IRFlow (IfStmt id' cond gotomain (Label (gototrue, exprs)) Nothing) : rest) old_lbl@(Label (_, label_exprs)) acc ir@IR{irCFlowIdx=idx} = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_if = IRFlow $ IfStmt id' cond gotomain (Label (labelled_true, exprs)) Nothing
      in 
      if length label_exprs == 0 
      then aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : acc) ir{irCFlowIdx=idx + 2}
      else aux rest (Label (labelSuffix $ idx + 2, [])) (labelled_if : (IRFlow $ Lbl old_lbl) : acc) ir{irCFlowIdx=idx + 2}
    aux (IRFlow (IfStmt id' cond gotomain (Label (gototrue, texprs)) (Just (Label (gotofalse, fexprs)))) : rest) old_lbl@(Label (_, label_exprs)) acc ir@IR{irCFlowIdx=idx} =
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_false = gotofalse <> (T.show $ idx + 2)
        labelled_if = IRFlow $ IfStmt id' cond gotomain (Label (labelled_true, texprs)) (Just (Label (labelled_false, fexprs)))
      in 
      if length label_exprs == 0 
      then aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : acc) ir{irCFlowIdx=idx + 3}
      else aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : (IRFlow $ Lbl old_lbl) : acc) ir{irCFlowIdx=idx + 3}

    aux (goto@(IRFlow (CGoto _)) : rest) old_lbl@(Label (_, exprs)) acc ir@IR{irCFlowIdx=idx} = 
      if length exprs == 0 
      then aux rest (Label (labelSuffix $ idx + 1, [])) (goto : acc) ir{irCFlowIdx=idx + 1}
      else aux rest (Label (labelSuffix $ idx + 1, [])) (goto : (IRFlow $ Lbl old_lbl) : acc) ir{irCFlowIdx=idx + 1}
    aux (expr : rest) curr_lbl acc ir = aux rest (pushExpr curr_lbl expr) acc ir

-- FIX: detect if there's nested if statements
patchJumps :: NE.NonEmpty IROp -> [IROp]
patchJumps = aux . NE.toList
  where
    aux :: [IROp] -> [IROp]
    aux [] = []
    aux [(IRFlow (IfStmt id' cond _ (Label (gototrue, exprs)) Nothing))] =
      let 
        lastE = getLast $ last exprs
        endlbl = IRFlow $ Lbl $ Label ("end", [IRExpr $ Phi "res" [(gototrue, lastE)]])
        patched_exprs = exprs <> [IRFlow $ CGoto "end"] 
      in
      IRFlow (IfStmt id' cond "end" (Label (gototrue, patched_exprs)) Nothing) : endlbl : []
    aux [(IRFlow (IfStmt id' cond _ (Label (gototrue, texprs)) (Just (Label (gotofalse, fexprs)))))] =
      let 
        (lastT, lastF) = (getLast $ last texprs, getLast $ last fexprs)
        endlbl = IRFlow $ Lbl $ Label ("end", [(IRExpr $ Phi "res" [(gototrue, lastT), (gotofalse, lastF)]), (IRExpr $ Ret (Ident "res"))])
        patched_texprs = texprs <> [IRFlow $ CGoto "end"] 
        patched_fexprs = fexprs <> [IRFlow $ CGoto "end"] 
      in
      IRFlow (IfStmt id' cond "end" (Label (gototrue, patched_texprs)) (Just (Label (gotofalse, patched_fexprs)))) : endlbl : []
    aux ((IRFlow (IfStmt id' cond _ (Label (gototrue, exprs)) Nothing)) : next@(IRFlow (Lbl (Label (n, _)))) : rest) =
      let patched_exprs = exprs <> [IRFlow $ CGoto n] in
      IRFlow (IfStmt id' cond n (Label (gototrue, patched_exprs)) Nothing) : next : (aux rest)
    aux ((IRFlow (IfStmt id' cond _ (Label (gototrue, texprs)) (Just (Label (gotofalse, fexprs))))) : next@(IRFlow (Lbl ((Label (n, _))))) : rest) =
      let 
        patched_texprs = texprs <> [IRFlow $ CGoto n] 
        patched_fexprs = fexprs <> [IRFlow $ CGoto n] 
      in
      IRFlow (IfStmt id' cond n (Label (gototrue, patched_texprs)) (Just (Label (gotofalse, patched_fexprs)))) : next : (aux rest) 
    aux (IRFlow (Lbl (Label (n, exprs))) : rest) = 
      let patched_body = reverse $ aux exprs in IRFlow (Lbl (Label (n, patched_body))) : aux rest
    aux (IRFlow (BlockStmt n exprs) : rest) = 
      let patched_body = aux exprs in IRFlow (BlockStmt n patched_body) : aux rest
    aux (expr : rest) = expr : aux rest

    getLast :: IROp -> Expr
    getLast (IRFlow (BlockStmt _ b)) = getLast $ last b
    getLast (IRExpr (Bin i _ _ _ _)) = Ident i
    getLast (IRExpr (Un i _ _ _)) = Ident i
    getLast (IRExpr op@(FuncCall _ _ _ _)) = op
    getLast (IRVar (Variable i _ _)) = Ident i
    getLast (IRExpr ret@(Ret _)) = ret
    getLast (IRExpr (Print _)) = EVal Unt
    getLast (IRFlow (IfStmt _ _ _ (Label (_, b)) Nothing)) = getLast $ last b
    -- TODO: do something for this case:
    -- getLast (IRIF _ (Label (_, bt)) (Just (Label (_, bf)))) = [getLast bt, getLast bf]
    getLast v = error $ "\nMalformed IR -\n  " <> (show v)

programToIR :: L.Program -> IR
programToIR (L.Program funcs glbls _) = 
  let
    ir = mkIR
    (glbls', ir') = glblsToIR (map (\(L.GlblVar _ v) -> v) glbls) ir []
    (funcs', ir''@IR{irAllocatedStrings=strs}) = funcsToIR funcs ir' []
  in ir''{irFuncs=funcs', irGlbls=glbls', irAllocatedStrings=reverse strs}
  where
    glblsToIR :: [L.Var] -> IR -> [Variable] -> ([Variable], IR)
    glblsToIR [] ir'' acc = (reverse acc, ir'')
    glblsToIR (L.Var{varIdent=i, varType=ty, varValue=v} : vs) ir'' acc =
      let 
        (expr, ir''') = fromExpr v ir''
        variable = Variable i ty expr
      in glblsToIR vs ir''' (variable : acc)

    funcsToIR :: [L.Func] -> IR -> [Fn] -> ([Fn], IR)
    funcsToIR [] ir'' acc = (reverse acc, ir'')
    funcsToIR ((L.Func ident _ args ret body) : fs) ir'' acc =
      let
        (body', ir''') = translateBody body ir''
        (body'', ir'''') = applyLabels body' ir'''
        body''' = patchJumps $ NE.fromList body''
        func = Fn ident args body''' ret
      in funcsToIR fs ir'''' (func : acc)

ppIR :: L.Program -> IO ()
ppIR prog =
  let ir@IR{irFuncs=funcs, irGlbls=glbls} = programToIR prog in
  putDoc $ PP.sep $ ((map PP.pretty glbls) <> (map PP.pretty funcs)) <> [border, formatIR ir]
  where
    border :: PP.Doc T.Text
    border = (PP.pretty @T.Text $ T.pack $ replicate 30 '-') <> PP.line

    -- NOTE: consider showing globals here?
    formatIR :: IR -> PP.Doc T.Text
    formatIR IR{irAllocatedStrings=strs} =
      let 
        contents =
          if length strs == 0 then [(PP.pretty @T.Text "<empty>")]
                              else formatAllocStrs strs
      in (PP.pretty @T.Text "DATA:") <> PP.line <> ((PP.indent 2 . PP.vsep) contents) <> PP.line

    formatAllocStrs :: [(T.Text, Int)] -> [PP.Doc T.Text]
    formatAllocStrs [] = []
    formatAllocStrs ((lit, idx): rest) =
      ((PP.pretty @T.Text "string[") <> (PP.viaShow idx) <> (PP.pretty @T.Text "] =") PP.<+> (PP.viaShow lit)) : formatAllocStrs rest
