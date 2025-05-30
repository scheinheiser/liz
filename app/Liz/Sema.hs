{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema where

import qualified Liz.Common.Error as E
import qualified Liz.Common.Types as L
import qualified Data.Map as M
import qualified Data.Text as T

import qualified Error.Diagnose as D

import Data.Char (isDigit)

data SymbolTbl = SymbolTbl 
  { symFuncMap  :: M.Map T.Text L.Func
  , symVarMap   :: M.Map T.Text L.Var
  , symConstMap :: M.Map T.Text L.Var
  } deriving (Show, Eq)

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

-- helper sema functions
collectErrors :: [Either [E.SemErr] L.Type] -> [E.SemErr] -> [L.Type] -> ([E.SemErr], [L.Type])
collectErrors [] errs types = (errs, types)
collectErrors (x : xs) errs types =
  case x of
    Left err -> collectErrors xs (err ++ errs) types
    Right t -> collectErrors xs errs (t : types)

-- main sema functions

-- temp testing function
testing :: L.Program -> FilePath -> IO ()
testing (L.Program prog) f = do
  let
    (res, hasMain) = aux prog mkSymTbl False []
    (errs, _) = collectErrors res [] []
  r <- readFile f
  let df = D.addFile D.def f r
  case () of _
              | length errs /= 0 -> E.printErrs errs df f
              | not hasMain -> E.printErrs [E.NoEntrypoint] df f
              | otherwise -> putStrLn "all good"
  where
    aux :: [L.SExpr] -> SymbolTbl -> Bool -> [Either [E.SemErr] L.Type] -> ([Either [E.SemErr] L.Type], Bool)
    aux [] _ hasMain acc = (acc, hasMain)
    aux (ex@(L.SEFunc L.Func{funcIdent=i}) : exprs) sym hasMain acc =
      let
        (res, next) = infer ex sym
      in if i == "main" then aux exprs next True (res : acc)
                        else aux exprs next hasMain (res : acc)
    aux (ex : exprs) sym hasMain acc =
      let
        (res, next) = infer ex sym
      in aux exprs next hasMain (res : acc)

-- TODO: Allow declarations in any order.
analyseProgram :: L.Program -> Either [E.SemErr] L.Program
analyseProgram p@(L.Program prog) = 
  let
    (res, hasMain) = aux prog mkSymTbl False []
    (errs, _) = collectErrors res [] []
  in
  case () of _
              | length errs /= 0 && not hasMain -> Left $ E.NoEntrypoint : errs
              | length errs /= 0 -> Left errs
              | not hasMain -> Left [E.NoEntrypoint]
              | otherwise -> Right p
  where
    aux :: [L.SExpr] -> SymbolTbl -> Bool -> [Either [E.SemErr] L.Type] -> ([Either [E.SemErr] L.Type], Bool)
    aux [] _ hasMain acc = (acc, hasMain)
    aux (ex@(L.SEFunc L.Func{funcIdent=i}) : exprs) sym hasMain acc =
      let
        (res, next) = infer ex sym
      in if i == "main" then aux exprs next True (res : acc)
                        else aux exprs next hasMain (res : acc)
    aux (ex : exprs) sym hasMain acc =
      let
        (res, next) = infer ex sym
      in aux exprs next hasMain (res : acc)

infer :: L.SExpr -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
infer (L.SEIdentifier iden s e) tbl = inferIdentifier s e iden tbl
infer (L.SELiteral lit s e) tbl = (inferLiteral s e lit, tbl)
infer (L.SEUnary op s e v) tbl = inferUnary s e op v tbl
infer (L.SEBinary op s e l r) tbl = inferBinary s e op l r tbl
infer (L.SEVar s e v) tbl = inferVariable s e v tbl False
infer (L.SEConst s e v) tbl = inferVariable s e v tbl True
infer (L.SESet s e i v) tbl = inferSet s e i v tbl
infer (L.SEReturn _ _ v) tbl = infer v tbl
infer (L.SEPrint _ _ v) tbl = infer v tbl
infer (L.SEFunc f) tbl = inferFunc f tbl
infer (L.SEFuncCall s e iden args) tbl = inferFuncCall s e iden args tbl
infer L.SEComment tbl = (Right L.String', tbl)
infer s tbl = (Left [E.NotImplemented s], tbl)

inferIdentifier :: L.LizPos -> L.LizPos -> T.Text -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferIdentifier s e iden tbl@(SymbolTbl {symFuncMap=ftbl, symVarMap=vtbl, symConstMap=ctbl}) =
  let
    func = iden `M.lookup` ftbl
    var = iden `M.lookup` vtbl
    constant = iden `M.lookup` ctbl
  in aux s e iden func var constant
  where
    aux st end i (Just _) (Just _) _ = (Left $ [E.IdentifierAlreadyInUse st end i], tbl)
    aux st end i (Just _) _ (Just _) = (Left $ [E.IdentifierAlreadyInUse st end i], tbl)
    aux st end i _ (Just _) (Just _) = (Left $ [E.IdentifierAlreadyInUse st end i], tbl)
    aux _ _ _ (Just L.Func{funcReturnType=ty}) _ _ = (Right ty, tbl)
    aux _ _ _ _ (Just L.Var{varType=ty}) _ = (Right ty, tbl)
    aux _ _ _ _ _ (Just L.Var{varType=ty}) = (Right ty, tbl)
    aux st end i Nothing Nothing Nothing = (Left $ [E.UndefinedIdentifier st end i], tbl)

inferLiteral :: L.LizPos -> L.LizPos -> T.Text -> Either [E.SemErr] L.Type
inferLiteral s e v
  | (count '.' v) == 1 =
    if (==) 0 $ (removeDigits . T.filter ((/=) '.')) v
    then pure L.Float'
    else Left $ [E.FailedLitInference s e v]
  | removeDigits v == 0 = pure L.Int'
  | (T.take 1 v) == "'" && (T.last v) == '\'' = pure L.Char'
  | (T.take 1 v) == "\"" && (T.last v) == '"' = pure L.String'
  | v == "True" || v == "False" = pure L.Bool'
  | v == "()" = pure L.Unit'
  | v == "undefined" = pure L.Undef'
  | otherwise = Left $ [E.FailedLitInference s e v]
  where
    removeDigits :: T.Text -> Int
    removeDigits = T.length . T.filter (not . isDigit)

    count :: Char -> T.Text -> Int
    count t = (T.length . T.filter (t ==))

inferUnary :: L.LizPos -> L.LizPos -> L.UnaryOp -> L.SExpr -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferUnary s e op v tbl =
  case (infer v tbl) of
    err@((Left _), _) -> err
    (Right operandType, t) -> (aux op operandType, t)
  where
    aux :: L.UnaryOp -> L.Type -> Either [E.SemErr] L.Type
    aux L.Negate L.Int' = Right L.Int'
    aux L.Negate L.Float' = Right L.Float'
    aux L.Negate t = Left $ [E.IncorrectTypes s e "Float or Int" [t]]

    aux L.Not L.Bool' = Right L.Bool'
    aux L.Not t = Left $ [E.IncorrectType s e L.Bool' t]

inferBinary :: L.LizPos -> L.LizPos -> L.BinaryOp -> L.SExpr -> L.SExpr -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferBinary s e op l r tbl =
  case (infer l tbl, infer r tbl) of
    (err@(Left _, _), _) -> err
    (_, err@(Left _, _)) -> err
    ((Right leftType, t1), (Right rightType, t2)) -> (aux op leftType rightType, t1 `combineSymTbl` t2)
  where
    aux :: L.BinaryOp -> L.Type -> L.Type -> Either [E.SemErr] L.Type
    aux L.Concat L.String' L.String' = Right L.String'
    aux L.Concat L.String' rt = Left $ [E.IncorrectType s e L.String' rt]
    aux L.Concat lt L.String' = Left $ [E.IncorrectType s e L.String' lt]
    aux L.Concat lt rt = Left $ [E.IncorrectTypes s e "String"  [lt, rt]]

    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Int' L.Int' = Right L.Int'
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) lt L.Int' = Left [E.IncorrectType s e L.Int' lt]
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Int' rt = Left [E.IncorrectType s e L.Int' rt]

    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Float' L.Float' = Right L.Float'
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) lt L.Float' = Left [E.IncorrectType s e L.Float' lt]
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Float' rt = Left [E.IncorrectType s e L.Float' rt]
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) lt rt = Left $ [E.IncorrectTypes s e "Float or Int" [lt, rt]]

    aux (L.Less; L.Greater; L.Eql; L.NotEql; L.GreaterEql; L.LessEql) lt rt
      | lt == rt = Right L.Bool'
      | otherwise = Left $ [E.MismatchedTypes s e lt (T.pack $ show rt)]

inferVariable :: L.LizPos -> L.LizPos -> L.Var -> SymbolTbl -> Bool -> (Either [E.SemErr] L.Type, SymbolTbl)
inferVariable s e var@L.Var{..} tbl isConst =
  case (infer varValue tbl) of
    err@(Left _, _) -> err
    (Right ty, ntbl) -> aux varIdent varType ty ntbl
  where
    aux :: T.Text -> L.Type -> L.Type -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
    aux ident decType valType t@(SymbolTbl {symVarMap=varMap, symConstMap=constMap, symFuncMap=funcMap})
      | ident `M.member` varMap || ident `M.member` constMap || ident `M.member` funcMap = (Left $ [E.IdentifierAlreadyInUse s e ident], tbl)
      | valType /= decType && valType /= L.Undef' = 
        let newTbl = M.insert ident var -- so that you don't get a bunch of undefined variable errors.
        in if isConst then (Left $ [E.MismatchedTypes s e decType (T.pack $ show valType)], t{symConstMap=newTbl constMap})
                      else (Left $ [E.MismatchedTypes s e decType (T.pack $ show valType)], t{symVarMap=newTbl varMap})
      | otherwise = 
        let newtbl = M.insert ident var 
        in if isConst then (Right decType, t{symConstMap=newtbl constMap})
                      else (Right decType, t{symVarMap=newtbl varMap})

inferSet :: L.LizPos -> L.LizPos -> T.Text -> L.SExpr -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferSet s e ident v tbl =
  case (infer v tbl) of
    err@(Left _, _) -> err
    (Right ty, ntbl) -> aux ident ty ntbl
  where
    aux :: T.Text -> L.Type -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
    aux i ty table@(SymbolTbl{..})
      | i `M.member` symConstMap = (Left $ [E.AssigningToConstant s e i], table)
      | i `M.member` symFuncMap = (Left $ [E.AssigningToFunction s e ident], table)
      | otherwise =
        let value = i `M.lookup` symVarMap in 
        case value of
          Nothing -> (Left $ [E.UndefinedIdentifier s e i], table)
          Just x | (L.varType x) == ty -> (Right ty, table)
                 | otherwise -> let correctType = L.varType x in (Left $ [E.IncorrectType s e correctType ty], table)

inferFunc :: L.Func -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferFunc f@(L.Func{..}) tbl@(SymbolTbl{..})
  | funcIdent `M.member` symFuncMap || funcIdent `M.member` symVarMap || funcIdent `M.member` symConstMap = (Left $ [E.IdentifierAlreadyInUse funcStart funcEnd funcIdent], tbl)
  | otherwise =
    let
      tblWithArgs = flip addArgs tbl $ map (\L.Arg{..} -> L.Var{varIdent=argIdent, varType=argType}) funcArgs
      (result, ntbl, vis, cis, fis) = evaluateFuncBody funcBody tblWithArgs
      errsAndTypes = collectErrors result [] []
    in aux (map L.argIdent funcArgs) vis cis fis errsAndTypes funcReturnType ntbl
  where
    aux argIdents vis cis fis (errs, types) ret table@(SymbolTbl {symConstMap=constMap, symVarMap=varMap, symFuncMap=funcMap}) =
      let
      -- removing anything declared within the function from the table.
          nixConsts = foldl' (flip M.delete) constMap (argIdents ++ cis)
          nixVars = foldl' (flip M.delete) varMap vis
          nixFuncs = foldl' (flip M.delete) funcMap fis
          nixFuncDefsTbl = table{symConstMap=nixConsts, symVarMap=nixVars, symFuncMap=nixFuncs}
      in 
      case () of _
                  | last types /= ret -> (Left $ [E.MismatchedTypes funcStart funcEnd ret (T.pack $ show (last types))], nixFuncDefsTbl)
                  | length errs /= 0 -> (Left errs, nixFuncDefsTbl)
                  | otherwise -> 
                      let newFuncMap = M.insert funcIdent f (nixFuncs)
                      in (Right ret, nixFuncDefsTbl{symFuncMap=newFuncMap})

    addArgs :: [L.Var] -> SymbolTbl -> SymbolTbl
    addArgs [] table = table
    addArgs (x : xs) table = addArgs xs $ addConst x table -- args are constant by default

    evaluateFuncBody :: [L.SExpr] -> SymbolTbl -> ([Either [E.SemErr] L.Type], SymbolTbl, [T.Text], [T.Text], [T.Text])
    evaluateFuncBody sexprs t = go sexprs t [] [] [] []
      where
        go [] table acc varIdents constIdents funcIdents = (acc, table, varIdents, constIdents, funcIdents)
        go (x : xs) table acc varIdents constIdents funcIdents = 
          let (res, nt) = infer x table 
          in case x of
            (L.SEVar _ _ L.Var{varIdent=i}) -> go xs nt (res : acc) (i : varIdents) constIdents funcIdents
            (L.SEConst _ _ L.Var{varIdent=i}) -> go xs nt (res : acc) varIdents (i : constIdents) funcIdents
            (L.SEFunc L.Func{funcIdent=i}) -> go xs nt (res : acc) varIdents constIdents (i : funcIdents)
            _ -> go xs nt (res : acc) varIdents constIdents funcIdents

inferFuncCall :: L.LizPos -> L.LizPos -> T.Text -> [L.SExpr] -> SymbolTbl -> (Either [E.SemErr] L.Type, SymbolTbl)
inferFuncCall s e ident sexprs tbl@(SymbolTbl{..})
  | ident `M.notMember` symFuncMap || ident `M.member` symConstMap || ident `M.member` symVarMap = (Left $ [E.UndefinedFunction s e ident], tbl)
  | otherwise =
    case (ident `M.lookup` symFuncMap) of
        Nothing -> (Left $ [E.UndefinedIdentifier s e ident], tbl)
        Just (L.Func{funcReturnType=retType, funcArgs=args}) ->
          let 
            evaluated_sexprs = map (fst . flip infer tbl) sexprs 
            argTypes = map L.argType args
            (errs, types) = collectErrors evaluated_sexprs [] []
          in if length errs /= 0 then (Left errs, tbl)
                                 else aux types argTypes retType
  where
    aux :: [L.Type] -> [L.Type] -> L.Type -> (Either [E.SemErr] L.Type, SymbolTbl)
    aux ts as ret 
      | length ts > length as = (Left $ [E.TooManyArgs s e ident (length ts - length as)], tbl)
      | length ts < length as = (Left $ [E.NotEnoughArgs s e ident (length as - length ts)], tbl) 
      | otherwise =
        let wts = findWrongTypes ts as in
        if length wts /= 0 then (Left $ [E.IncorrectArgTypes s e ident as wts], tbl)
                           else (Right ret, tbl)

    findWrongTypes :: [L.Type] -> [L.Type] -> [L.Type]
    findWrongTypes [] _ = []
    findWrongTypes (x : xs) ys | not $ x `elem` ys = x : findWrongTypes xs ys
                               | otherwise = findWrongTypes xs ys
