{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema.Terminators (inspectTerminators) where

import qualified Liz.Common.Types as CT
import qualified Liz.Common.Errors as E

import qualified Data.List.NonEmpty as NE

import Data.Either (lefts, rights)
import Data.Foldable (fold)

inspectTerminators :: CT.Program -> Either [E.SemErr] CT.Program
inspectTerminators (CT.Program funcs glbls macros) =
  let
    funcs' = checkReturns . insertReturns $ funcs
  in
  if ((length . lefts $ funcs') /= 0)
  then Left $ fold . lefts $ funcs'
  else Right $ CT.Program (rights funcs') glbls macros

insertReturns :: [CT.Func] -> [CT.Func]
insertReturns = map aux
  where
    aux :: CT.Func -> CT.Func
    aux f@CT.Func{funcBody=body} =
      let 
        l = 
          case (last body) of
            CT.SEExpr e -> CT.SEExpr $ handleExpr e 
            CT.SEFlow f -> CT.SEFlow $ handleFlow f
            expr -> expr
        body' = (init body) <> [l]
      in f{CT.funcBody=body'}

    handleSExpr :: CT.SExpr -> CT.SExpr
    handleSExpr (CT.SEExpr e) = CT.SEExpr $ handleExpr e 
    handleSExpr (CT.SEFlow f) = CT.SEFlow $ handleFlow f
    handleSExpr sexpr = sexpr

    handleFlow :: CT.ControlFlow -> CT.ControlFlow
    handleFlow (CT.FBlockStmt r body) =
      let
        init' = NE.init $ body
        last' = handleSExpr $ NE.last body
      in CT.FBlockStmt r (NE.fromList $ init' <> [last'])
    handleFlow (CT.FIfStmt r cond tbranch Nothing) =
      let tbranch' = handleSExpr tbranch in
      CT.FIfStmt r cond tbranch' Nothing
    handleFlow (CT.FIfStmt r cond tbranch (Just fbranch)) =
      let 
        tbranch' = handleSExpr tbranch 
        fbranch' = handleSExpr fbranch 
      in
      CT.FIfStmt r cond tbranch' (Just fbranch')

    handleExpr :: CT.Expression -> CT.Expression
    handleExpr v@(CT.EBinary _ r _ _) = CT.EReturn r v
    handleExpr v@(CT.EUnary _ r _) = CT.EReturn r v
    handleExpr v@(CT.EFuncCall r _ _) = CT.EReturn r v
    handleExpr v@(CT.EValueMacro _ r) = CT.EReturn r v
    handleExpr e = e

checkReturns :: [CT.Func] -> [Either [E.SemErr] CT.Func]
checkReturns [] = []
checkReturns (f@CT.Func{funcBody=body} : fs) =
  case (last body) of
    (CT.SESet range _ _) -> Left [E.InvalidReturn range] : checkReturns fs
    (CT.SEVar range _) -> Left [E.InvalidReturn range] : checkReturns fs
    (CT.SEConst range _) -> Left [E.InvalidReturn range] : checkReturns fs
    _ -> Right f : checkReturns fs
