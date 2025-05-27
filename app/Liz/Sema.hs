{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema where

import qualified Liz.Common.Error as E
import qualified Liz.Common.Types as L
import qualified Data.Map as M
import qualified Data.Text as T

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
    symFuncMap  = symF1 `M.union` symF2
    symVarMap   = symV1 `M.union` symV2
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

infer :: L.SExpr -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
infer (L.SEIdentifier p iden) tbl = inferIdentifier p iden tbl
infer (L.SELiteral p lit) tbl = (inferLiteral p lit, tbl)
infer (L.SEUnary p op v) tbl = inferUnary p op v tbl
infer (L.SEBinary p op l r) tbl = inferBinary p op l r tbl
infer (L.SEVar p v) tbl = inferVariable p v tbl False
infer (L.SEConst p v) tbl = inferVariable p v tbl True
infer (L.SESet p i v) tbl = inferSet p i v tbl
infer (L.SEReturn _ v) tbl = infer v tbl
infer (L.SEPrint _ v) tbl = infer v tbl
infer (L.SEFunc f) tbl = inferFunc f tbl
infer (L.SEFuncCall p iden args) tbl = inferFuncCall p iden args tbl
infer L.SEComment tbl = undefined
infer s tbl = (Left [E.NotImplemented s], tbl)

inferIdentifier :: L.LizPos -> T.Text -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferIdentifier pos iden tbl@(SymbolTbl {symFuncMap=ftbl, symVarMap=vtbl, symConstMap=ctbl}) =
  let
    func = iden `M.lookup` ftbl
    var = iden `M.lookup` vtbl
    constant = iden `M.lookup` ctbl
  in aux pos iden func var constant
  where
    aux p i (Just _) (Just _) _ = (Left $ [E.IdentifierAlreadyInUse p i], tbl)
    aux p i (Just _) _ (Just _) = (Left $ [E.IdentifierAlreadyInUse p i], tbl)
    aux p i _ (Just _) (Just _) = (Left $ [E.IdentifierAlreadyInUse p i], tbl)
    aux _ _ (Just L.Func{funcReturnType=ty}) _ _ = (Right ty, tbl)
    aux _ _ _ (Just L.Var{varType=ty}) _ = (Right ty, tbl)
    aux _ _ _ _ (Just L.Var{varType=ty}) = (Right ty, tbl)
    aux p i Nothing Nothing Nothing = (Left $ [E.UndefinedIdentifier p i], tbl)

inferLiteral :: L.LizPos -> T.Text -> Either [E.SemErr] L.Type
inferLiteral pos v
  | (count '.' v) == 1 =
    if (==) 0 $ (removeDigits . T.filter ((/=) '.')) v
    then pure L.Float'
    else Left $ [E.FailedLitInference pos v]
  | removeDigits v == 0 = pure L.Int'
  | (T.take 1 v) == "'" && (T.last v) == '\'' = pure L.Char'
  | (T.take 1 v) == "\"" && (T.last v) == '"' = pure L.String'
  | v == "True" || v == "False" = pure L.Bool'
  | v == "()" = pure L.Unit'
  | otherwise = Left $ [E.FailedLitInference pos v]
  where
    removeDigits :: T.Text -> Int
    removeDigits = T.length . T.filter (not . isDigit)

    count :: Char -> T.Text -> Int
    count t = (T.length . T.filter (t ==))

inferUnary :: L.LizPos -> L.UnaryOp -> L.SExpr -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferUnary p op v tbl =
  case (infer v tbl) of
    e@((Left _), _) -> e
    (Right operandType, t) -> (aux op operandType, t)
  where
    aux :: L.UnaryOp -> L.Type -> Either [E.SemErr] L.Type
    aux L.Negate L.Int' = Right L.Int'
    aux L.Negate L.Float' = Right L.Float'
    aux L.Negate t = Left $ [E.IncorrectTypes p "Float or Int" [t]]

    aux L.Not L.Bool' = Right L.Bool'
    aux L.Not t = Left $ [E.IncorrectType p L.Bool' t]

inferBinary :: L.LizPos -> L.BinaryOp -> L.SExpr -> L.SExpr -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferBinary p op l r tbl =
  case (infer l tbl, infer r tbl) of
    (e@(Left _, _), _) -> e
    (_, e@(Left _, _)) -> e
    ((Right leftType, t1), (Right rightType, t2)) -> (aux op leftType rightType, t1 `combineSymTbl` t2)
  where
    aux :: L.BinaryOp -> L.Type -> L.Type -> Either [E.SemErr] L.Type
    aux L.Concat L.String' L.String' = Right L.String'
    aux L.Concat L.String' rt = Left $ [E.IncorrectType p L.String' rt]
    aux L.Concat lt L.String' = Left $ [E.IncorrectType p L.String' lt]
    aux L.Concat lt rt = Left $ [E.IncorrectTypes p "String"  [lt, rt]]

    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Int' L.Int' = Right L.Int'
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Float' L.Float' = Right L.Float'
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) lt rt = Left $ [E.IncorrectTypes p "Float or Int" [lt, rt]]

    aux (L.Less; L.Greater; L.Eql; L.NotEql; L.GreaterEql; L.LessEql) lt rt
      | lt == rt = Right L.Bool'
      | otherwise = Left $ [E.MismatchedTypes p lt (T.pack $ show rt)]

inferVariable :: L.LizPos -> L.Var -> SymbolTbl -> Bool -> (Either [E.SemErr] L.Type, SymbolTbl)
inferVariable p var@L.Var{..} tbl isConst =
  case (infer varValue tbl) of
    err@(Left _, _) -> err
    (Right ty, ntbl) -> aux varIdent varType ty ntbl
  where
    aux :: T.Text -> L.Type -> L.Type -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
    aux ident valType decType t@(SymbolTbl {symVarMap=varMap, symConstMap=constMap, symFuncMap=funcMap})
      | ident `M.member` varMap || ident `M.member` constMap || ident `M.member` funcMap = (Left $ [E.IdentifierAlreadyInUse p ident], tbl)
      | valType /= decType = (Left $ [E.MismatchedTypes p decType (T.pack $ show valType)], tbl)
      | isConst =
        let newTbl = M.insert ident var constMap in
        (Right decType, t{symConstMap=newTbl})
      | otherwise = 
        let newtbl = M.insert ident var varMap in
        (Right decType, t{symVarMap=newtbl})

inferSet :: L.LizPos -> T.Text -> L.SExpr -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferSet p ident v tbl =
  case (infer v tbl) of
    err@(Left _, _) -> err
    (Right ty, ntbl) -> aux ident ty ntbl
  where
    aux :: T.Text -> L.Type -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
    aux i ty table@(SymbolTbl{..})
      | i `M.member` symConstMap = (Left $ [E.AssigningToConstant p i], table)
      | i `M.member` symFuncMap = (Left $ [E.AssigningToFunction p ident], table)
      | otherwise =
        let value = i `M.lookup` symVarMap in 
        case value of
          Nothing -> (Left $ [E.UndefinedIdentifier p i], table)
          Just x | (L.varType x) == ty -> (Right ty, table)
                 | otherwise -> let correctType = L.varType x in (Left $ [E.IncorrectType p correctType ty], table)

inferFunc :: L.Func -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferFunc f@(L.Func{..}) tbl@(SymbolTbl{..})
  | funcIdent `M.member` symFuncMap || funcIdent `M.member` symVarMap || funcIdent `M.member` symConstMap = (Left $ [E.IdentifierAlreadyInUse funcPos funcIdent], tbl)
  | otherwise =
    let
      tblWithArgs = flip addArgs tbl $ map (\L.Arg{..} -> L.Var{varIdent=argIdent, varType=argType}) funcArgs
      (result, ntbl) = evaluateFuncBody funcBody tblWithArgs
      errsAndTypes = collectErrors result [] []
    in aux (map L.argIdent funcArgs) errsAndTypes funcReturnType ntbl
  where
    aux :: [T.Text] -> ([E.SemErr], [L.Type]) -> L.Type -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
    aux argIdents (errs, types) ret table@(SymbolTbl {symConstMap=constMap})
      | length errs /= 0 = (Left errs, table)
      | last types /= ret = (Left $ [E.MismatchedTypes funcPos ret (T.pack $ show (last types))], table)
      | otherwise = 
        let 
          newConstMap = foldr M.delete constMap argIdents 
          ntbl = (addFunc f table) {symConstMap=newConstMap}
        in (Right ret, ntbl)

    addArgs :: [L.Var] -> SymbolTbl -> SymbolTbl
    addArgs [] table = table
    addArgs (x : xs) table = addArgs xs $ addConst x table

    evaluateFuncBody :: [L.SExpr] -> SymbolTbl -> ([Either [E.SemErr] L.Type], SymbolTbl)
    evaluateFuncBody sexprs t = go sexprs t []
      where
        go [] table acc = (acc, table)
        go (x : xs) table acc = let (res, nt) = infer x table in go xs nt (res : acc)

    collectErrors :: [Either [E.SemErr] L.Type] -> [E.SemErr] -> [L.Type] -> ([E.SemErr], [L.Type])
    collectErrors [] errs types = (errs, types)
    collectErrors (x : xs) errs types =
      case x of
      Left e -> collectErrors xs (e ++ errs) types
      Right t -> collectErrors xs errs (t : types)

inferFuncCall :: L.LizPos -> T.Text -> [L.SExpr] -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferFuncCall p ident sexprs tbl@(SymbolTbl{..})
  | ident `M.notMember` symFuncMap || ident `M.member` symConstMap || ident `M.member` symVarMap = (Left $ [E.NotAFunction p ident], tbl)
  | otherwise =
    let value = ident `M.lookup` symFuncMap in
    case value of
        Nothing -> (Left $ [E.UndefinedIdentifier p ident], tbl)
        Just (L.Func{funcReturnType=retType, funcArgs=args}) ->
          let 
            evaluated_sexprs = map (fst . flip infer tbl) sexprs 
            argTypes = map L.argType args
            (errs, types) = collectErrors evaluated_sexprs [] []
          in 
          if length errs /= 0 
          then (Left errs, tbl)
          else aux types argTypes retType
  where
    aux :: [L.Type] -> [L.Type] -> L.Type -> (Either [E.SemErr] L.Type, SymbolTbl)
    aux ts as ret 
      | length ts > length as = (Left $ [E.TooManyArgs p ident], tbl)
      | length ts < length as = (Left $ [E.NotEnoughArgs p ident], tbl) 
      | otherwise =
        let wts = notInBoth ts as in
        if length wts /= 0 
        then (Left $ [E.IncorrectArgTypes p ident wts], tbl)
        else (Right ret, tbl)

    notInBoth :: [L.Type] -> [L.Type] -> [L.Type]
    notInBoth [] _ = []
    notInBoth (x : xs) ys | not $ x `elem` ys = x : notInBoth xs ys
                          | otherwise = notInBoth xs ys

    collectErrors :: [Either [E.SemErr] L.Type] -> [E.SemErr] -> [L.Type] -> ([E.SemErr], [L.Type])
    collectErrors [] errs types = (errs, types)
    collectErrors (x : xs) errs types =
      case x of
        Left e -> collectErrors xs (e ++ errs) types
        Right t -> collectErrors xs errs (t : types)
