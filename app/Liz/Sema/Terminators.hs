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
    aux f@CT.Func{funcIdent="main"} = f
    aux f@CT.Func{funcBody=body} =
      let 
        l = handleSExpr . last $ body
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
checkReturns (f@CT.Func{funcIdent="main", funcBody=body, funcReturnType=CT.Int'} : fs) =
  case (last body) of
    (CT.SESet range _ _) -> Left [E.InvalidReturn range] : checkReturns fs
    (CT.SEVar range _) -> Left [E.InvalidReturn range] : checkReturns fs
    (CT.SEConst range _) -> Left [E.InvalidReturn range] : checkReturns fs
    _ -> Right f : checkReturns fs
checkReturns (f@CT.Func{funcIdent="main", funcBody=body, funcReturnType=ret, funcPos=range} : fs)
  | ret /= CT.Unit' = Left [E.NonUnitOrIntMain] : checkReturns fs
  | hasReturn body = Left [E.ReturnInUnitMain] : checkReturns fs
  | otherwise =
    let body' = body <> [CT.SEExpr . CT.EReturn range $ CT.ELiteral CT.Int' "0" range] in
    Right f{CT.funcBody=body'} : checkReturns fs
  where
    hasReturn :: [CT.SExpr] -> Bool
    hasReturn [] = False
    hasReturn (CT.SEFlow (CT.FBlockStmt _ block) : rest) = (hasReturn . NE.toList $ block) || hasReturn rest
    hasReturn (CT.SEFlow (CT.FIfStmt _ _ true Nothing) : rest) = (hasReturn [true]) || hasReturn rest
    hasReturn (CT.SEFlow (CT.FIfStmt _ _ true (Just false)) : rest) = (hasReturn [true]) || (hasReturn [false]) || hasReturn rest
    hasReturn (CT.SESet _ _ e : rest) = (hasReturn [CT.SEExpr e]) || hasReturn rest
    hasReturn (CT.SEVar _ CT.Var{varValue=e} : rest) = (hasReturn [CT.SEExpr e]) || hasReturn rest
    hasReturn (CT.SEConst _ CT.Var{varValue=e} : rest) = (hasReturn [CT.SEExpr e]) || hasReturn rest
    hasReturn (CT.SEExpr (CT.EBinary _ _ l r) : rest) = (hasReturn [CT.SEExpr l]) || (hasReturn [CT.SEExpr r]) || hasReturn rest
    hasReturn (CT.SEExpr (CT.EUnary _ _ v) : rest) = (hasReturn [CT.SEExpr v]) || hasReturn rest
    hasReturn (CT.SEExpr (CT.EPrint _ v) : rest) = (hasReturn [CT.SEExpr v]) || hasReturn rest
    hasReturn (CT.SEExpr (CT.EFuncCall _ _ vs) : rest) = (hasReturn . map CT.SEExpr $ vs) || hasReturn rest
    hasReturn (CT.SEExpr (CT.EReturn _ _) : rest) = True || hasReturn rest
    hasReturn (_ : rest) = False || hasReturn rest
checkReturns (f@CT.Func{funcBody=body} : fs) =
  case (last body) of
    (CT.SESet range _ _) -> Left [E.InvalidReturn range] : checkReturns fs
    (CT.SEVar range _) -> Left [E.InvalidReturn range] : checkReturns fs
    (CT.SEConst range _) -> Left [E.InvalidReturn range] : checkReturns fs
    _ -> Right f : checkReturns fs
