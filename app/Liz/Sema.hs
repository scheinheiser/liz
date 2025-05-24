{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Liz.Sema where

import qualified Liz.Common.Error as E
import qualified Liz.Common.Types as L
import qualified Liz.Parser as P
import qualified Data.Map as M
import qualified Data.Text as T

import Text.Megaparsec hiding (count)
import Data.Char (isDigit)

data SymbolTbl = SymbolTbl 
  { symFuncMap  :: M.Map T.Text L.Func
  , symVarMap   :: M.Map T.Text L.Var
  , symConstMap :: M.Map T.Text L.Var
  }

mkSymTbl :: SymbolTbl
mkSymTbl = SymbolTbl {symFuncMap = M.empty, symVarMap = M.empty, symConstMap = M.empty}

combineSymTbl :: SymbolTbl -> SymbolTbl -> SymbolTbl
combineSymTbl (SymbolTbl {symFuncMap=symF1, symVarMap=symV1, symConstMap=symC1}) (SymbolTbl {symFuncMap=symF2, symVarMap=symV2, symConstMap=symC2}) =
  let
    symFuncMap = symF1 `M.union` symF2
    symVarMap = symV1 ` M.union` symV2
    symConstMap = symC1 `M.union` symC2
  in SymbolTbl {symFuncMap, symVarMap, symConstMap}

addFunc :: L.Func -> SymbolTbl -> SymbolTbl
addFunc f@(L.Func {funcIdent=ident}) tbl@(SymbolTbl {symFuncMap=ftbl}) = tbl {symFuncMap = M.insert ident f ftbl}

addVar :: L.Var -> SymbolTbl -> SymbolTbl
addVar v@(L.Var {varIdent=ident}) tbl@(SymbolTbl {symVarMap=vtbl}) = tbl {symVarMap = M.insert ident v vtbl}

addConst :: L.Var -> SymbolTbl -> SymbolTbl
addConst c@(L.Var {varIdent=ident}) tbl@(SymbolTbl {symConstMap=ctbl}) = tbl {symConstMap = M.insert ident c ctbl}

-- analyseProgram :: P.Program -> Either E.SemErr P.Program
-- analyseProgram (P.Program prog) = map infer prog

infer :: L.SExpr -> SymbolTbl -> Either E.SemErr L.SExpr
infer (L.Identifier p iden) tbl = inferIdentifier p iden tbl
infer (L.SELiteral p lit) _ = inferLitType p lit
  where
    inferIdentifier :: L.LizPos -> T.Text -> SymbolTbl -> Either E.SemErr L.SExpr
    inferIdentifier pos iden (SymbolTbl {symFuncMap=ftbl, symVarMap=vtbl, symConstMap=ctbl}) =
      if (iden `M.member` fbtl) || (iden `M.member` vtbl) || (iden `M.member` ctbl)
      then pure $ L.Identifier pos iden
      else Left $ E.UndefinedIdentifier pos iden

    --TODO: Look at this one again, I don't like returning the type.
    inferLitType :: L.LizPos -> T.Text -> Either E.SemErr L.SExpr
    inferLitType pos v
      | (count '.' v) == 1 =
        if (==) 0 $ (removeDigits . T.filter ((/=) '.')) v
        then pure $ L.SEType L.Float'
        else Left $ E.FailedLitInference pos v
      | removeDigits v == 0 = pure $ L.SEType L.Int'
      | (T.take 1 v) == "'" && (T.last v) == '\'' = pure $ L.SEType L.Char'
      | (T.take 1 v) == "\"" && (T.last v) == '"' = pure $ L.SEType L.String'
      | v == "True" || v == "False" = pure $ L.SEType L.Bool'
      | v == "()" = pure $ L.SEType L.Unit'
      | otherwise = Left $ E.FailedLitInference pos v
      where
        removeDigits :: T.Text -> Int
        removeDigits = T.length . T.filter (not . isDigit)

        count :: Char -> T.Text -> Int
        count t = (T.length . T.filter (t ==))
