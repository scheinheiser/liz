{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OrPatterns #-}

module Liz.IR.IR (ppIR, programToIR, flattenExpr) where

import Liz.IR.IRTypes
import qualified Liz.Common.Types as L

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.List (mapAccumL)

import qualified Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)

-- helper functions
labelSuffix :: Int -> T.Text
labelSuffix index = "L" <> (T.show index)

translateBody :: [L.SExpr] -> IR -> ([CFlow], IR)
translateBody l ir@IR{irCFlowIdx=i} = aux l ir{irCFlowIdx=i+1} (Label (labelSuffix i, [])) []
  where
    aux :: [L.SExpr] -> IR -> Label -> [CFlow] -> ([CFlow], IR)
    aux [] ir' (Label (n, exprs)) acc = 
      if length exprs /= 0 then (Lbl (Label (n, reverse exprs)) : acc, ir')
                           else (acc, ir')
    aux (sexpr : rest) ir' lbl acc =
      let (flow, ir'', lbl') = fromSExpr sexpr ir' lbl in
      aux rest ir'' lbl' (acc <> flow)

tempVarIdent :: Int -> T.Text
tempVarIdent i = T.pack $ "t" <> (show i)

mkIR :: IR
mkIR = IR [] [] [] 0 0 0 M.empty

getType :: M.Map T.Text L.Type -> Expr -> L.Type
getType _ (EVal (Integ _)) = L.Int'
getType _ (EVal (Flt _)) = L.Float'
getType _ (EVal (Bln _)) = L.Bool'
getType _ (EVal (Str _ _)) = L.String'
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
getType m (Ident i _) = m M.! i
getType m (FuncCall _ _ i _) = m M.! i
getType _ e = error $ "called getType with unexpected expr: " <> (show e)

-- main ir functions
fromSExpr :: L.SExpr -> IR -> Label -> ([CFlow], IR, Label)
fromSExpr (L.SEExpr ex) ir (Label (n, exprs)) =
  let (ex', ir') = fromExpr ex ir in
  ([], ir', Label (n, (IRExpr ex') : exprs))
fromSExpr (L.SEFlow flow) ir old_lbl@(Label (_, exprs)) =
  let (ex', ir'@IR{irCFlowIdx=i}) = fromCFlow flow ir in
  if length exprs /= 0 
  then ([Lbl old_lbl, ex'], ir'{irCFlowIdx=i + 1}, Label (labelSuffix i, []))
  else ([ex'], ir'{irCFlowIdx=i + 1}, Label (labelSuffix i, []))
fromSExpr (L.SEVar _ L.Var{varIdent = ident, varType = t, varValue = v}) ir (Label (n, exprs)) =
  let 
    (v', ir'@IR{irSymbols=symmap}) = fromExpr v ir 
    var = IRVar $ Variable ident t v'
    symmap' = ident `M.insert` t $ symmap
  in
  ([], ir'{irSymbols=symmap'}, Label (n, var : exprs))
fromSExpr (L.SESet _ ident v) ir@IR{irSymbols=symmap} (Label (n, exprs)) =
  let 
    (v', ir') = fromExpr v ir 
    var = IRVar $ Variable ident (symmap M.! ident) v'
  in
  ([], ir', Label (n, var : exprs))
fromSExpr (L.SEConst _ L.Var{varIdent = ident, varType = t, varValue = v}) ir@IR{irSymbols=symmap} (Label (n, exprs)) =
  let 
    (v', ir') = fromExpr v ir
    symmap' = ident `M.insert` t $ symmap
    var = IRVar $ Variable ident t v'
  in
  ([], ir'{irSymbols=symmap'}, Label (n, var : exprs))

fromCFlow :: L.ControlFlow -> IR -> (CFlow, IR)
fromCFlow (L.FIfStmt _ cond tbranch Nothing) ir@IR{irCFlowIdx=fi} =
  let 
    flowidx = "if$" <> (T.show fi)
    (cond', ir') = fromExpr cond ir
    (_, ir'', tlbl) = fromSExpr tbranch ir' (Label (labelSuffix fi, []))
    gotomain = "blank" -- blank goto because labels haven't been applied to the rest.
  in (IfStmt flowidx cond' gotomain tlbl Nothing, ir''{irCFlowIdx=fi + 1})
fromCFlow (L.FIfStmt _ cond tbranch (Just fbranch)) ir@IR{irCFlowIdx=fi} =
  let 
    flowidx = "if$" <> (T.show fi)
    (cond', ir') = fromExpr cond ir
    (_, ir'', tlbl) = fromSExpr tbranch ir' (Label (labelSuffix fi, []))
    (_, ir''', flbl) = fromSExpr fbranch ir'' (Label (labelSuffix $ fi + 1, []))
    gotomain = "blank"
  in (IfStmt flowidx cond' gotomain tlbl (Just flbl), ir'''{irCFlowIdx=fi + 2})
fromCFlow (L.FBlockStmt _ vals) ir@IR{irCFlowIdx=fi} =
  let 
    flowidx = "block$" <> (T.show fi)
    (body, ir') = translateBody vals ir
  in
  (BlockStmt flowidx body, ir'{irCFlowIdx=fi + 1})

fromExpr :: L.Expression -> IR -> (Expr, IR)
fromExpr (L.EIdentifier ident _) ir@IR{irGlbls=glbls} = 
  if any (ident ==) (map (\(Variable n _ _) -> n) glbls) then (Ident ident True, ir)
                                                         else (Ident ident False, ir)
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
          let 
            i' = "str" <> (T.show i)
            allocStrs = (lit, i') : strs in
          (EVal $ Str i' lit, Just $ ir{irAllocatedStrings=allocStrs, irStringIdx=i + 1})
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
    ty = getType symmap (Ident ident False)
  in
  (FuncCall temp_i ty ident params', ir'''{irTempVarIdx=i + 1})

-- FIX: detect if there's nested if statements
-- TODO: add 'end' label suffixes.
patchJumps :: NE.NonEmpty CFlow -> IR -> [CFlow]
patchJumps = aux . NE.toList
  where
    aux :: [CFlow] -> IR -> [CFlow]
    aux [] _ = []
    aux (stmt : rest@((IfStmt n _ _ _ _) : _)) ir = (patchCFlow stmt n ir) : (aux rest ir)
    aux (stmt : rest@((Lbl (Label (n, _))) : _)) ir = (patchCFlow stmt n ir) : (aux rest ir)
    aux (stmt : rest@((BlockStmt n _) : _)) ir = (patchCFlow stmt n ir) : (aux rest ir)
    aux [(IfStmt id' cond _ (Label (gototrue, exprs)) Nothing)] IR{irSymbols=symmap} =
      let 
        lastE = getLast $ last exprs
        endlbl = 
          if (getType symmap lastE) == L.Unit' 
          then Lbl $ Label ("end", [IRExpr $ Ret (EVal Unt)])
          else Lbl $ Label ("end", [(IRExpr $ Phi "res" (getType symmap lastE) [(gototrue, lastE)]), (IRExpr $ Ret (Ident "res" False))])
        patched_exprs = exprs <> [IRGoto "end"] 
      in
      (IfStmt id' cond "end" (Label (gototrue, patched_exprs)) Nothing) : endlbl : []
    aux [(IfStmt id' cond _ (Label (gototrue, texprs)) (Just (Label (gotofalse, fexprs))))] IR{irSymbols=symmap} =
      let 
        (lastT, lastF) = (getLast $ last texprs, getLast $ last fexprs)
        endlbl = 
          if (getType symmap lastT) == L.Unit'
          then Lbl $ Label ("end", [IRExpr $ Ret (EVal Unt)])
          else Lbl $ Label ("end", [(IRExpr $ Phi "res" (getType symmap lastT) [(gototrue, lastT), (gotofalse, lastF)]), (IRExpr $ Ret (Ident "res" False))])
        patched_texprs = texprs <> [IRGoto "end"] 
        patched_fexprs = fexprs <> [IRGoto "end"] 
      in
      (IfStmt id' cond "end" (Label (gototrue, patched_texprs)) (Just (Label (gotofalse, patched_fexprs)))) : endlbl : []
    aux (expr : rest) ir = expr : aux rest ir

    patchCFlow :: CFlow -> T.Text -> IR -> CFlow
    patchCFlow (IfStmt id' cond _ (Label (gototrue, exprs)) Nothing) next _ =
      let patched_exprs = exprs <> [IRGoto next] in
      (IfStmt id' cond next (Label (gototrue, patched_exprs)) Nothing)
    patchCFlow (IfStmt id' cond _ (Label (gototrue, texprs)) (Just (Label (gotofalse, fexprs)))) next _ =
      let 
        patched_texprs = texprs <> [IRGoto next] 
        patched_fexprs = fexprs <> [IRGoto next] 
      in (IfStmt id' cond next (Label (gototrue, patched_texprs)) (Just (Label (gotofalse, patched_fexprs))))
    patchCFlow (BlockStmt id' exprs) next ir =
      let 
        patched_exprs = aux exprs ir 
        gotoend = [Lbl $ Label ("end", [IRGoto next])]
        patched_exprs' =
          case (last patched_exprs) of
            Lbl (Label ("end", _)) -> (init patched_exprs) <> gotoend
            _ -> aux (patched_exprs <> gotoend) ir
      in
      (BlockStmt id' patched_exprs')
    patchCFlow (Lbl (Label (n, exprs))) next _ =
      let patched_exprs = exprs <> [IRGoto next] in
      (Lbl $ Label (n, patched_exprs))

    getLast :: IROp -> Expr 
    getLast (IRExpr (Bin i _ _ _ _)) = Ident i False
    getLast (IRExpr (Un i _ _ _)) = Ident i False
    getLast (IRExpr op@(FuncCall _ _ _ _)) = op
    getLast (IRVar (Variable i _ _)) = Ident i False
    getLast (IRExpr ret@(Ret _)) = ret
    getLast (IRExpr (Print _)) = EVal Unt
    getLast v = error $ "\nMalformed IR -\n  " <> (show v)

-- TODO: change to return nonempty
flattenExpr :: Expr -> [Expr]
flattenExpr (Bin i t op l@(Bin li _ _ _ _) r@(Bin ri _ _ _ _)) = (flattenExpr l) <> (flattenExpr r) <> [(Bin i t op (Ident li False) (Ident ri False))]
flattenExpr (Bin i t op l@(Bin li _ _ _ _) r@(Un ri _ _ _)) = (flattenExpr l) <> (flattenExpr r) <> [(Bin i t op (Ident li False) (Ident ri False))] 
flattenExpr (Bin i t op l@(Un li _ _ _) r@(Bin ri _ _ _ _)) = (flattenExpr l) <> (flattenExpr r) <> [(Bin i t op (Ident li False) (Ident ri False))] 
flattenExpr (Bin i t op l@(Un li _ _ _) r@(Un ri _ _ _)) = (flattenExpr l) <> (flattenExpr r) <> [(Bin i t op (Ident li False) (Ident ri False))] 
flattenExpr (Bin i t op l@(FuncCall li _ _ _) r@(Bin ri _ _ _ _)) = (flattenExpr l) <> (flattenExpr r) <> [(Bin i t op (Ident li False) (Ident ri False))] 
flattenExpr (Bin i t op l@(FuncCall li _ _ _) r@(Un ri _ _ _)) = (flattenExpr l) <> (flattenExpr r) <> [(Bin i t op (Ident li False) (Ident ri False))] 
flattenExpr (Bin i t op l@(Bin li _ _ _ _) r@(FuncCall ri _ _ _)) = (flattenExpr l) <> (flattenExpr r) <> [(Bin i t op (Ident li False) (Ident ri False))] 
flattenExpr (Bin i t op l@(Un li _ _ _) r@(FuncCall ri _ _ _)) = (flattenExpr l) <> (flattenExpr r) <> [(Bin i t op (Ident li False) (Ident ri False))] 
flattenExpr b@(Bin _ _ _ _ _) = [b]
flattenExpr (Un i t op v@(Bin vi _ _ _ _)) = (flattenExpr v) <> [(Un i t op (Ident vi False))]
flattenExpr (Un i t op v@(Un vi _ _ _)) = (flattenExpr v) <> [(Un i t op (Ident vi False))] 
flattenExpr (Un i t op v@(FuncCall vi _ _ _)) = (flattenExpr v) <> [(Un i t op (Ident vi False))] 
flattenExpr u@(Un _ _ _ _) = [u]
flattenExpr (Ret v@(Bin vi _ _ _ _)) = (flattenExpr v) <> [(Ret $ Ident vi False)]
flattenExpr (Ret v@(Un vi _ _ _)) = (flattenExpr v) <> [(Ret $ Ident vi False)]
flattenExpr (Ret v@(FuncCall vi _ _ _)) = (flattenExpr v) <> [(Ret $ Ident vi False)]
flattenExpr r@(Ret _) = [r]
flattenExpr (Print v@(Bin vi _ _ _ _)) = (flattenExpr v) <> [(Print $ Ident vi False)]
flattenExpr (Print v@(Un vi _ _ _)) = (flattenExpr v) <> [(Print $ Ident vi False)]
flattenExpr (Print v@(FuncCall vi _ _ _)) = (flattenExpr v) <> [(Print $ Ident vi False)]
flattenExpr r@(Print _) = [r]
flattenExpr expr = [expr] -- TODO: cover phi and funccall cases.

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
        body'' = patchJumps (NE.fromList body') ir'''
        body''' = flattenBody body''
        func = Fn ident args (body''') ret
      in funcsToIR fs ir''' (func : acc)

    flattenIROp :: [IROp] -> [IROp]
    flattenIROp [] = []
    flattenIROp (IRExpr e : is) =
      let flattened_expr = flattenExpr e in
      (map IRExpr flattened_expr) <> flattenIROp is
    flattenIROp (IRVar (Variable i t (e@((Bin _ _ _ _ _);(Un _ _ _ _);(FuncCall _ _ _ _)))) : is) =
      let flattened_expr = flattenExpr e in
      if length flattened_expr > 1 
      then (map IRExpr $ init flattened_expr) <> ((IRVar $ Variable i t (last flattened_expr)) : flattenIROp is)
      else ((IRVar $ Variable i t (last flattened_expr)) : flattenIROp is)
    flattenIROp (i : is) = i : flattenIROp is

    flattenBody :: [CFlow] -> [CFlow]
    flattenBody [] = []
    flattenBody ((Lbl (Label (n, exprs))) : cs) = Lbl (Label (n, flattenIROp exprs)) : (flattenBody cs)
    flattenBody ((BlockStmt i exprs) : cs) = BlockStmt i (flattenBody exprs) : (flattenBody cs)
    flattenBody (IfStmt i cond main (Label (tlbl, texprs)) (Just (Label (flbl, fexprs))) : cs) =
      -- we delay flattening the condition.
      -- doing it now presents some cases which I think aren't solvable/would be annoying to catch (e.g. back-to-back if statements)
      IfStmt i cond main (Label (tlbl, flattenIROp texprs)) (Just $ Label (flbl, flattenIROp fexprs)) : flattenBody cs
    flattenBody ((IfStmt i cond main (Label (tlbl, texprs)) Nothing) : cs) =
      IfStmt i cond main (Label (tlbl, flattenIROp texprs)) Nothing : flattenBody cs

ppIR :: L.Program -> IO ()
ppIR prog =
  let ir@IR{irFuncs=funcs} = programToIR prog in
  putDoc $ PP.sep $ (map PP.pretty funcs) <> [border, formatIR ir]
  where
    border :: PP.Doc T.Text
    border = (PP.pretty @T.Text $ T.pack $ replicate 30 '-') <> PP.line

    formatIR :: IR -> PP.Doc T.Text
    formatIR IR{irGlbls=glbls, irAllocatedStrings=strs} =
      let 
        glbl_contents =
          if length glbls == 0 then [(PP.pretty @T.Text "<empty>")]
                               else map PP.pretty glbls
        data_contents =
          if length strs == 0 then [(PP.pretty @T.Text "<empty>")]
                              else formatAllocStrs strs
      in 
      (PP.pretty @T.Text "GLOBALS:") <> PP.line
        <> ((PP.indent 2 . PP.vsep) glbl_contents) <> PP.line
           <> PP.line <> (PP.pretty @T.Text "DATA:")
            <> PP.line <> ((PP.indent 2 . PP.vsep) data_contents) 
              <> PP.line

    formatAllocStrs :: [(T.Text, T.Text)] -> [PP.Doc T.Text]
    formatAllocStrs [] = []
    formatAllocStrs ((lit, idx): rest) =
      ((PP.pretty @T.Text "string[") <> (PP.pretty $ T.drop 3 idx) <> (PP.pretty @T.Text "] =") PP.<+> (PP.viaShow lit)) : formatAllocStrs rest
