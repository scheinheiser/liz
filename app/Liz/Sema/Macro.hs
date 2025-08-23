{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema.Macro where

import qualified Liz.Common.Errors as E
import qualified Liz.Common.Types as CT
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Data.Either (lefts, rights)
import Data.Foldable (fold)
import Data.List (mapAccumL)

type MacroTbl = M.Map T.Text CT.Expression

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

populateMacroTbl :: [CT.Macro] -> Either [E.SemErr] MacroTbl
populateMacroTbl macros = aux macros mkMacroTbl []
  where
    aux :: [CT.Macro] -> MacroTbl -> [E.SemErr] -> Either [E.SemErr] MacroTbl
    aux [] tbl errs =
      if length errs /= 0 then Left $ reverse errs
                          else Right tbl
    aux (CT.Macro{..} : ms) tbl errs
      | macIdent `M.member` tbl = aux ms tbl (E.IdentifierAlreadyInUse macPos macIdent : errs)
      | checkRecursiveDef macValue macIdent = aux ms tbl (E.RecursiveMacroDef macPos macIdent : errs)
      | otherwise = aux ms (macIdent `M.insert` macValue $ tbl) errs

    checkRecursiveDef :: CT.Expression -> T.Text -> Bool
    checkRecursiveDef (CT.EValueMacro call_ident _) def_ident = call_ident == def_ident
    checkRecursiveDef (CT.EFuncCall _ _ params) def_ident = any (== True) (map (flip checkRecursiveDef def_ident) params)
    checkRecursiveDef (CT.EReturn _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (CT.EPrint _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (CT.EBinary _ _ l r) def_ident = (checkRecursiveDef l def_ident) || (checkRecursiveDef r def_ident)
    checkRecursiveDef (CT.EUnary _ _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef _ _ = False

subBody :: (a -> MacroTbl -> (Either [E.SemErr] a, MacroTbl)) -> [a] -> MacroTbl -> [Either [E.SemErr] a]
subBody subFunc exprs tbl =
  snd . fst $ 
    mapAccumL 
      (\(tbl', acc) expr -> 
        let (res, tbl'') = subFunc expr tbl' in 
        ((tbl'', res : acc), expr)) (tbl, []) exprs

simpleSub :: (CT.Expression -> a) -> CT.Expression -> MacroTbl -> (Either [E.SemErr] a, MacroTbl)
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

multiSub :: (a -> MacroTbl -> (Either [E.SemErr] a, MacroTbl)) -> ([a] -> a) -> [a] -> MacroTbl -> (Either [E.SemErr] a, MacroTbl)
multiSub subFunc dec body tbl =
  let 
    subbed_body = subBody subFunc body tbl 
    (errs, subbed_body')  = collectErrors subbed_body
  in
  if length errs /= 0 
  then (Left errs, tbl)
  else
    let filtered_body = dec subbed_body' in
    (Right filtered_body, tbl)

-- main macro sub functions
substituteMacros :: CT.Program -> Either [E.SemErr] CT.Program
substituteMacros (CT.Program funcs glbls macros) = do
  tbl <- populateMacroTbl macros
  glbls' <- 
    (let (errs, glbls'') = collectErrors $ subGlbls glbls tbl in
     if length errs /= 0 then Left errs
                         else Right glbls'')
  funcs' <- 
    (let (errs, funcs'') = collectErrors $ subFuncs funcs tbl in
     if length errs /= 0 then Left errs
                         else Right funcs'')
  pure $ CT.Program funcs' glbls' []
  where
    subGlbls :: [CT.GlblVar] -> MacroTbl -> [Either [E.SemErr] CT.GlblVar]
    subGlbls [] _ = []
    subGlbls (gvar@(CT.GlblVar r var@CT.Var{varValue=v}) : gs) tbl =
      case v of
        CT.EValueMacro i range ->
          let value = i `M.lookup` tbl in
          case value of
            Just sub -> (Right $ CT.GlblVar r var{CT.varValue=sub}) : subGlbls gs tbl
            Nothing -> (Left $ [E.UndefinedIdentifier range i]) : subGlbls gs tbl
        _ -> (Right gvar) : subGlbls gs tbl

    subFuncs :: [CT.Func] -> MacroTbl -> [Either [E.SemErr] CT.Func]
    subFuncs [] _ = []
    subFuncs (f@CT.Func{funcBody=body} : fs) tbl =
      let (errs, body') = collectErrors $ subBody macroSub body tbl in
      if length errs /= 0 then (Left errs) : subFuncs fs tbl
                          else (Right f{CT.funcBody=body'}) : subFuncs fs tbl

macroSub :: CT.SExpr -> MacroTbl -> (Either [E.SemErr] CT.SExpr, MacroTbl)
macroSub (CT.SEExpr ex) tbl = let (res, tbl') = macroSubExpr ex tbl in (CT.SEExpr <$> res, tbl')
macroSub (CT.SEFlow (CT.FBlockStmt range body)) tbl = multiSub @CT.SExpr macroSub (CT.SEFlow . CT.FBlockStmt range . NE.fromList) (NE.toList body) tbl
macroSub (CT.SEVar range (CT.Var i t v)) tbl = simpleSub @CT.SExpr (\x -> CT.SEVar range (CT.Var i t x)) v tbl
macroSub (CT.SEConst range (CT.Var i t v)) tbl = simpleSub @CT.SExpr (\x -> CT.SEConst range (CT.Var i t x)) v tbl
macroSub (CT.SESet range i v) tbl = simpleSub @CT.SExpr (CT.SESet range i) v tbl
macroSub (CT.SEFlow (CT.FIfStmt range cond branch Nothing)) tbl =
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
                  expr = CT.SEFlow $ CT.FIfStmt range filtered_cond filtered_branch Nothing
                in (Right expr, tbl)
macroSub (CT.SEFlow (CT.FIfStmt range cond tbranch (Just fbranch))) tbl =
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
                  expr = CT.SEFlow $ CT.FIfStmt range filtered_cond filtered_tbranch (Just filtered_fbranch)
                in (Right expr, tbl)
macroSub v tbl = (Right v, tbl)

macroSubExpr :: CT.Expression -> MacroTbl -> (Either [E.SemErr] CT.Expression, MacroTbl)
macroSubExpr (CT.EFuncCall range i params) tbl = multiSub @CT.Expression macroSubExpr (CT.EFuncCall range i) params tbl
macroSubExpr (CT.EPrint range v) tbl = simpleSub @CT.Expression (CT.EPrint range) v tbl
macroSubExpr (CT.EReturn range v) tbl = simpleSub @CT.Expression (CT.EReturn range) v tbl
macroSubExpr (CT.EValueMacro i range) tbl =
  let value = i `M.lookup` tbl in
  case value of
    Nothing -> (Left [E.UndefinedIdentifier range i], tbl)
    Just expr -> (Right expr, tbl)
macroSubExpr (CT.EBinary op range l r) tbl =
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
                  expr = CT.EBinary op range filtered_left filtered_right
                in (Right expr, tbl)
macroSubExpr (CT.EUnary op range v) tbl = simpleSub @CT.Expression (CT.EUnary op range) v tbl
macroSubExpr v tbl = (Right v, tbl)
