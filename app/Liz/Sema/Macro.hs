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
import Data.Maybe (catMaybes)
import Data.Foldable (fold)

type MacroTbl = M.Map T.Text L.Expression

collectErrors :: [Either [E.SemErr] a] -> ([E.SemErr], [a])
collectErrors l = (fold $ lefts l, rights l)

-- helper macro functions
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
infixr 5 <$$>

mkMacroTbl :: MacroTbl
mkMacroTbl = M.empty

head' :: [a] -> a
head' = NE.head . NE.fromList

-- if there's a 'none' value, then it must be a macro definition.
hasMacroDef :: Eq a => [Maybe a] -> Bool
hasMacroDef = (0 /=) . length . filter (Nothing ==)

subBody :: [L.SExpr] -> MacroTbl -> [Either [E.SemErr] (Maybe L.SExpr)]
subBody exprs t = reverse $ aux exprs t
  where
    aux :: [L.SExpr] -> MacroTbl -> [Either [E.SemErr] (Maybe L.SExpr)]
    aux [] _ = []
    aux (x : xs) tbl = let (res, tbl') = macroSub x tbl in res : aux xs tbl'

simpleSub :: (L.Expression -> L.SExpr) -> L.LizRange -> L.Expression -> MacroTbl -> (Either [E.SemErr] (Maybe L.SExpr), MacroTbl)
simpleSub dec range v tbl =
  let 
    (subbed_value, _) = macroSubExpr v tbl 
    (value_errs, subbed_value') = collectErrors [subbed_value]
  in
  case () of _
              | length value_errs /= 0 -> (Left value_errs, tbl)
              | hasMacroDef subbed_value' -> (Left [E.NonGlblMacroDef range], tbl)
              | otherwise ->
                let filtered_value = head' $ catMaybes subbed_value' in
                (Right (Just $ dec filtered_value), tbl)

simpleSubExpr :: (L.Expression -> L.Expression) -> L.LizRange -> L.Expression -> MacroTbl -> (Either [E.SemErr] (Maybe L.Expression), MacroTbl)
simpleSubExpr dec range v tbl =
  let 
    (subbed_value, _) = macroSubExpr v tbl 
    (value_errs, subbed_value') = collectErrors [subbed_value]
  in
  case () of _
              | length value_errs /= 0 -> (Left value_errs, tbl)
              | hasMacroDef subbed_value' -> (Left [E.NonGlblMacroDef range], tbl)
              | otherwise ->
                let filtered_value = head' $ catMaybes subbed_value' in
                (Right (Just $ dec filtered_value), tbl)

multiSub :: ([L.SExpr] -> L.SExpr) -> L.LizRange -> [L.SExpr] -> MacroTbl -> (Either [E.SemErr] (Maybe L.SExpr), MacroTbl)
multiSub dec range body tbl =
  let 
    subbed_body = subBody body tbl 
    (errs, subbed_body')  = collectErrors subbed_body
  in
  case () of _
              | length errs /= 0 -> (Left errs, tbl)
              | hasMacroDef subbed_body' -> (Left [E.NonGlblMacroDef range], tbl)
              | otherwise ->
                let filtered_body = dec $ catMaybes subbed_body' in
                (Right $ Just filtered_body, tbl)

multiSubExpr :: ([L.Expression] -> L.Expression) -> L.LizRange -> [L.Expression] -> MacroTbl -> (Either [E.SemErr] (Maybe L.Expression), MacroTbl)
multiSubExpr dec range body tbl =
  let 
    subbed_body = subBodyExpr body tbl 
    (errs, subbed_body')  = collectErrors subbed_body
  in
  case () of _
              | length errs /= 0 -> (Left errs, tbl)
              | hasMacroDef subbed_body' -> (Left [E.NonGlblMacroDef range], tbl)
              | otherwise ->
                let filtered_body = dec $ catMaybes subbed_body' in
                (Right $ Just filtered_body, tbl)
  where
    subBodyExpr :: [L.Expression] -> MacroTbl -> [Either [E.SemErr] (Maybe L.Expression)]
    subBodyExpr exprs t = reverse $ aux exprs t
      where
        aux :: [L.Expression] -> MacroTbl -> [Either [E.SemErr] (Maybe L.Expression)]
        aux [] _ = []
        aux (x : xs) tbl' = let (res, tbl'') = macroSubExpr x tbl' in res : aux xs tbl''

-- main macro sub function
macroSub :: L.SExpr -> MacroTbl -> (Either [E.SemErr] (Maybe L.SExpr), MacroTbl)
macroSub (L.SEMacroDef (L.Macro range i expr)) tbl
            | i `M.member` tbl = (Left [E.IdentifierAlreadyInUse range i], tbl)
            -- to stop knock-on undefined macro errors. since it just inserts the sexpr, there's no actual issue with infinite expansion in the table.
            | checkRecursiveDef expr i = let newMTbl = M.insert i expr tbl in (Left [E.RecursiveMacroDef range i], newMTbl)
            | otherwise = let newMTbl = M.insert i expr tbl in (Right Nothing, newMTbl)
  where
    checkRecursiveDef :: L.Expression -> T.Text -> Bool
    checkRecursiveDef (L.EValueMacro call_ident _) def_ident = call_ident == def_ident
    checkRecursiveDef (L.EFuncCall _ _ params) def_ident = any (== True) (map (flip checkRecursiveDef def_ident) params)
    checkRecursiveDef (L.EReturn _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.EPrint _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.EBinary _ _ l r) def_ident = (checkRecursiveDef l def_ident) || (checkRecursiveDef r def_ident)
    checkRecursiveDef (L.EUnary _ _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef _ _ = False
macroSub (L.SEFlow (L.FFunc f@(L.Func _ range _ _ body))) tbl = multiSub (\x -> L.SEFlow $ L.FFunc $ f{L.funcBody = x}) range body tbl
macroSub (L.SEExpr ex) tbl = let (res, tbl') = macroSubExpr ex tbl in (L.SEExpr <$$> res, tbl')
macroSub (L.SEFlow (L.FBlockStmt range body)) tbl = multiSub (L.SEFlow . L.FBlockStmt range) range body tbl
macroSub (L.SEVar range (L.Var i t v)) tbl = simpleSub (\x -> L.SEVar range (L.Var i t x)) range v tbl
macroSub (L.SEConst range (L.Var i t v)) tbl = simpleSub (\x -> L.SEConst range (L.Var i t x)) range v tbl
macroSub (L.SESet range i v) tbl = simpleSub (L.SESet range i) range v tbl
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
                  filtered_cond = head' $ catMaybes subbed_cond'
                  filtered_branch = head' $ catMaybes subbed_branch'
                  expr = L.SEFlow $ L.FIfStmt range filtered_cond filtered_branch Nothing
                in (Right $ Just expr, tbl)
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
              | hasMacroDef subbed_cond'
                || hasMacroDef subbed_tbranch'
                  || hasMacroDef subbed_fbranch' -> (Left [E.NonGlblMacroDef range], tbl)
              | otherwise ->
                let
                  filtered_cond = head' $ catMaybes subbed_cond'
                  filtered_tbranch = head' $ catMaybes subbed_tbranch'
                  filtered_fbranch = head' $ catMaybes subbed_fbranch'
                  expr = L.SEFlow $ L.FIfStmt range filtered_cond filtered_tbranch (Just filtered_fbranch)
                in (Right $ Just expr, tbl)
macroSub v tbl = (Right $ Just v, tbl)

macroSubExpr :: L.Expression -> MacroTbl -> (Either [E.SemErr] (Maybe L.Expression), MacroTbl)
macroSubExpr (L.EFuncCall range i params) tbl = multiSubExpr (L.EFuncCall range i) range params tbl
macroSubExpr (L.EPrint range v) tbl = simpleSubExpr (L.EPrint range) range v tbl
macroSubExpr (L.EReturn range v) tbl = simpleSubExpr (L.EReturn range) range v tbl
macroSubExpr (L.EValueMacro i range) tbl =
  let value = i `M.lookup` tbl in
  case value of
    Nothing -> (Left [E.UndefinedIdentifier range i], tbl)
    Just expr -> (Right (Just expr), tbl)
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
              | hasMacroDef subbed_left'
                || hasMacroDef subbed_right' -> (Left [E.NonGlblMacroDef range], tbl)
              | otherwise ->
                let 
                  filtered_left = head' $ catMaybes subbed_left'
                  filtered_right = head' $ catMaybes subbed_right'
                  expr = L.EBinary op range filtered_left filtered_right
                in (Right $ Just expr, tbl)
macroSubExpr (L.EUnary op range v) tbl = simpleSubExpr (L.EUnary op range) range v tbl
macroSubExpr v tbl = (Right $ Just v, tbl)
