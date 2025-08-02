{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema.Macro where

import qualified Liz.Common.Errors as E
import qualified Liz.Common.Types as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Data.Either (lefts, rights)
import Data.Foldable (fold)
import Data.List (mapAccumL)

type MacroTbl = M.Map T.Text L.Expression

collectErrors :: [Either [E.SemErr] a] -> ([E.SemErr], [a])
collectErrors l = (fold $ lefts l, rights l)

-- helper macro functions
mkMacroTbl :: MacroTbl
mkMacroTbl = M.empty

head' :: [a] -> a
head' = NE.head . NE.fromList

-- if there's a 'none' value, then it must be a macro definition.
hasMacroDef :: Eq a => [Maybe a] -> Bool
hasMacroDef = (0 /=) . length . filter (Nothing ==)

populateMacroTbl :: [L.Macro] -> Either [E.SemErr] MacroTbl
populateMacroTbl macros = aux macros mkMacroTbl []
  where
    aux :: [L.Macro] -> MacroTbl -> [E.SemErr] -> Either [E.SemErr] MacroTbl
    aux [] tbl errs =
      if length errs /= 0 then Left $ reverse errs
                          else Right tbl
    aux (L.Macro{..} : ms) tbl errs
      | macIdent `M.member` tbl = aux ms tbl (E.IdentifierAlreadyInUse macPos macIdent : errs)
      | checkRecursiveDef macValue macIdent = aux ms tbl (E.RecursiveMacroDef macPos macIdent : errs)
      | otherwise = aux ms (macIdent `M.insert` macValue $ tbl) errs

    checkRecursiveDef :: L.Expression -> T.Text -> Bool
    checkRecursiveDef (L.EValueMacro call_ident _) def_ident = call_ident == def_ident
    checkRecursiveDef (L.EFuncCall _ _ params) def_ident = any (== True) (map (flip checkRecursiveDef def_ident) params)
    checkRecursiveDef (L.EReturn _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.EPrint _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.EBinary _ _ l r) def_ident = (checkRecursiveDef l def_ident) || (checkRecursiveDef r def_ident)
    checkRecursiveDef (L.EUnary _ _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef _ _ = False

subBody :: [L.SExpr] -> MacroTbl -> [Either [E.SemErr] L.SExpr]
subBody exprs tbl =
  snd . fst $ 
    mapAccumL 
      (\(tbl', acc) expr -> 
        let (res, tbl'') = macroSub expr tbl' in 
        ((tbl'', res : acc), expr)) (tbl, []) exprs

simpleSub :: (L.Expression -> L.SExpr) -> L.Expression -> MacroTbl -> (Either [E.SemErr] L.SExpr, MacroTbl)
simpleSub dec v tbl =
  let 
    (subbed_value, _) = macroSubExpr v tbl 
    (value_errs, subbed_value') = collectErrors [subbed_value]
  in
  if length value_errs /= 0 
  then (Left value_errs, tbl)
  else 
    let filtered_value = head' subbed_value' in
    (Right $ dec filtered_value, tbl)

simpleSubExpr :: (L.Expression -> L.Expression) -> L.Expression -> MacroTbl -> (Either [E.SemErr] L.Expression, MacroTbl)
simpleSubExpr dec v tbl =
  let 
    (subbed_value, _) = macroSubExpr v tbl 
    (value_errs, subbed_value') = collectErrors [subbed_value]
  in
  if length value_errs /= 0 
  then (Left value_errs, tbl)
  else
    let filtered_value = head' subbed_value' in
    (Right $ dec filtered_value, tbl)

multiSub :: ([L.SExpr] -> L.SExpr) -> [L.SExpr] -> MacroTbl -> (Either [E.SemErr] L.SExpr, MacroTbl)
multiSub dec body tbl =
  let 
    subbed_body = subBody body tbl 
    (errs, subbed_body')  = collectErrors subbed_body
  in
  if length errs /= 0 
  then (Left errs, tbl)
  else
    let filtered_body = dec subbed_body' in
    (Right filtered_body, tbl)

multiSubExpr :: ([L.Expression] -> L.Expression) -> [L.Expression] -> MacroTbl -> (Either [E.SemErr] L.Expression, MacroTbl)
multiSubExpr dec body tbl =
  let 
    subbed_body = subBodyExpr body tbl 
    (errs, subbed_body')  = collectErrors subbed_body
  in
  if length errs /= 0 
  then (Left errs, tbl)
  else 
    let filtered_body = dec subbed_body' in
    (Right filtered_body, tbl)
  where
    subBodyExpr :: [L.Expression] -> MacroTbl -> [Either [E.SemErr] L.Expression]
    subBodyExpr exprs t = reverse $ aux exprs t
      where
        aux :: [L.Expression] -> MacroTbl -> [Either [E.SemErr] L.Expression]
        aux [] _ = []
        aux (x : xs) tbl' = let (res, tbl'') = macroSubExpr x tbl' in res : aux xs tbl''

-- main macro sub functions
substituteMacros :: L.Program -> Either [E.SemErr] L.Program
substituteMacros (L.Program funcs glbls macros) = do
  tbl <- populateMacroTbl macros
  glbls' <- 
    (let (errs, glbls'') = collectErrors $ subGlbls glbls tbl in
     if length errs /= 0 then Left errs
                         else Right glbls'')
  funcs' <- 
    (let (errs, funcs'') = collectErrors $ subFuncs funcs tbl in
     if length errs /= 0 then Left errs
                         else Right funcs'')
  pure $ L.Program funcs' glbls' []
  where
    subGlbls :: [L.GlblVar] -> MacroTbl -> [Either [E.SemErr] L.GlblVar]
    subGlbls [] _ = []
    subGlbls (gvar@(L.GlblVar r var@L.Var{varValue=v}) : gs) tbl =
      case v of
        L.EValueMacro i range ->
          let value = i `M.lookup` tbl in
          case value of
            Just sub -> (Right $ L.GlblVar r var{L.varValue=sub}) : subGlbls gs tbl
            Nothing -> (Left $ [E.UndefinedIdentifier range i]) : subGlbls gs tbl
        _ -> (Right gvar) : subGlbls gs tbl

    subFuncs :: [L.Func] -> MacroTbl -> [Either [E.SemErr] L.Func]
    subFuncs [] _ = []
    subFuncs (f@L.Func{funcBody=body} : fs) tbl =
      let (errs, body') = collectErrors $ subBody body tbl in
      if length errs /= 0 then (Left errs) : subFuncs fs tbl
                          else (Right f{L.funcBody=body'}) : subFuncs fs tbl

macroSub :: L.SExpr -> MacroTbl -> (Either [E.SemErr] L.SExpr, MacroTbl)
macroSub (L.SEExpr ex) tbl = let (res, tbl') = macroSubExpr ex tbl in (L.SEExpr <$> res, tbl')
macroSub (L.SEFlow (L.FBlockStmt range body)) tbl = multiSub (L.SEFlow . L.FBlockStmt range) body tbl
macroSub (L.SEVar range (L.Var i t v)) tbl = simpleSub (\x -> L.SEVar range (L.Var i t x)) v tbl
macroSub (L.SEConst range (L.Var i t v)) tbl = simpleSub (\x -> L.SEConst range (L.Var i t x)) v tbl
macroSub (L.SESet range i v) tbl = simpleSub (L.SESet range i) v tbl
macroSub (L.SEFlow (L.FIfStmt range cond branch Nothing)) tbl =
  let
    (subbed_cond, _) = macroSubExpr cond tbl
    (subbed_branch, _) = macroSub branch tbl
    (cond_err, subbed_cond') = collectErrors [subbed_cond]
    (branch_err, subbed_branch') = collectErrors [subbed_branch]
  in
  case () of _
              | length cond_err /= 0 && length branch_err /= 0 -> (Left $ cond_err <> branch_err, tbl)
              | length cond_err /= 0 -> (Left cond_err, tbl)
              | length branch_err /= 0 -> (Left branch_err, tbl)
              | otherwise ->
                let
                  filtered_cond = head' subbed_cond'
                  filtered_branch = head' subbed_branch'
                  expr = L.SEFlow $ L.FIfStmt range filtered_cond filtered_branch Nothing
                in (Right expr, tbl)
macroSub (L.SEFlow (L.FIfStmt range cond tbranch (Just fbranch))) tbl =
  let
    (subbed_cond, _) = macroSubExpr cond tbl
    (subbed_tbranch, _) = macroSub tbranch tbl
    (subbed_fbranch, _) = macroSub fbranch tbl
    (cond_err, subbed_cond') = collectErrors [subbed_cond]
    (tbranch_err, subbed_tbranch') = collectErrors [subbed_tbranch]
    (fbranch_err, subbed_fbranch') = collectErrors [subbed_fbranch]
  in
  case () of _
              | length cond_err /= 0 
                && length tbranch_err /= 0 
                  && length fbranch_err /= 0 -> (Left $ cond_err <> tbranch_err <> fbranch_err, tbl)
              | length cond_err /= 0 
                && length tbranch_err /= 0 -> (Left $ cond_err <> tbranch_err, tbl)
              | length cond_err /= 0 
                && length fbranch_err /= 0 -> (Left $ cond_err <> fbranch_err, tbl)
              | length tbranch_err /= 0 
                && length fbranch_err /= 0 -> (Left $ tbranch_err <> fbranch_err, tbl)
              | length cond_err /= 0 -> (Left cond_err, tbl)
              | length tbranch_err /= 0 -> (Left tbranch_err, tbl)
              | length fbranch_err /= 0 -> (Left fbranch_err, tbl)
              | otherwise ->
                let
                  filtered_cond = head' subbed_cond'
                  filtered_tbranch = head' subbed_tbranch'
                  filtered_fbranch = head' subbed_fbranch'
                  expr = L.SEFlow $ L.FIfStmt range filtered_cond filtered_tbranch (Just filtered_fbranch)
                in (Right expr, tbl)
macroSub v tbl = (Right v, tbl)

macroSubExpr :: L.Expression -> MacroTbl -> (Either [E.SemErr] L.Expression, MacroTbl)
macroSubExpr (L.EFuncCall range i params) tbl = multiSubExpr (L.EFuncCall range i) params tbl
macroSubExpr (L.EPrint range v) tbl = simpleSubExpr (L.EPrint range) v tbl
macroSubExpr (L.EReturn range v) tbl = simpleSubExpr (L.EReturn range) v tbl
macroSubExpr (L.EValueMacro i range) tbl =
  let value = i `M.lookup` tbl in
  case value of
    Nothing -> (Left [E.UndefinedIdentifier range i], tbl)
    Just expr -> (Right expr, tbl)
macroSubExpr (L.EBinary op range l r) tbl =
  let
    (subbed_left, _) = macroSubExpr l tbl
    (subbed_right, _) = macroSubExpr r tbl
    (left_err, subbed_left') = collectErrors [subbed_left]
    (right_err, subbed_right') = collectErrors [subbed_right]
  in
  case () of _
              | length left_err /= 0 
                && length right_err /= 0 -> (Left $ left_err <> right_err, tbl)
              | length left_err /= 0 -> (Left left_err, tbl)
              | length right_err /= 0 -> (Left right_err, tbl)
              | otherwise ->
                let 
                  filtered_left = head' subbed_left'
                  filtered_right = head' subbed_right'
                  expr = L.EBinary op range filtered_left filtered_right
                in (Right expr, tbl)
macroSubExpr (L.EUnary op range v) tbl = simpleSubExpr (L.EUnary op range) v tbl
macroSubExpr v tbl = (Right v, tbl)
