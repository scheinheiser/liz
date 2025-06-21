{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema where

import qualified Liz.Common.Error as E
import qualified Liz.Common.Types as L
import qualified Data.Map as M
import qualified Data.Text as T

data Env = Env 
  { envFuncs  :: M.Map T.Text (L.Type, [L.Type])
  , envVars   :: M.Map T.Text L.Type
  , envConsts :: M.Map T.Text L.Type
  } deriving (Show, Eq)

-- helper sema functions
mkEnv :: Env
mkEnv = Env {envFuncs = M.empty, envVars = M.empty, envConsts = M.empty}

combineEnv :: Env -> Env -> Env
combineEnv (Env {envFuncs=symF1, envVars=symV1, envConsts=symC1}) (Env {envFuncs=symF2, envVars=symV2, envConsts=symC2}) =
  let
    envFuncs  = symF1 `M.union` symF2
    envVars   = symV1 `M.union` symV2
    envConsts = symC1 `M.union` symC2
  in Env {envFuncs, envVars, envConsts}

collectErrors :: [Either [E.SemErr] L.Type] -> [E.SemErr] -> [L.Type] -> ([E.SemErr], [L.Type])
collectErrors [] errs types = (errs, types)
collectErrors (x : xs) errs types =
  case x of
    Left err -> collectErrors xs (err ++ errs) types
    Right t -> collectErrors xs errs (t : types)

-- main sema functions
testing :: L.Program -> FilePath -> IO ()
testing (L.Program prog) f = do
  let
    (res, hasMain) = aux prog mkEnv False []
    (errs, _) = collectErrors res [] []
  case () of _
              | length errs /= 0 && not hasMain -> E.printErrs f (E.NoEntrypoint : errs) []
              | length errs /= 0 -> E.printErrs f errs []
              | not hasMain -> E.printErrs f errs []
              | otherwise -> putStrLn "all good"
  where
    aux :: [L.SExpr] -> Env -> Bool -> [Either [E.SemErr] L.Type] -> ([Either [E.SemErr] L.Type], Bool)
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
    (res, hasMain) = aux prog mkEnv False []
    (errs, _) = collectErrors res [] []
  in
  case () of _
              | length errs /= 0 && not hasMain -> Left $ E.NoEntrypoint : errs
              | length errs /= 0 -> Left errs
              | not hasMain -> Left [E.NoEntrypoint]
              | otherwise -> Right p
  where
    aux :: [L.SExpr] -> Env -> Bool -> [Either [E.SemErr] L.Type] -> ([Either [E.SemErr] L.Type], Bool)
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

infer :: L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
infer (L.SEIdentifier iden s e) env = inferIdentifier s e iden env
infer (L.SELiteral ty _ _ _) env = (Right ty, env)
infer (L.SEUnary op s e v) env = inferUnary s e op v env
infer (L.SEBinary op s e l r) env = inferBinary s e op l r env
infer (L.SEVar s e v) env = inferVariable s e v env False
infer (L.SEConst s e v) env = inferVariable s e v env True
infer (L.SESet s e i v) env = inferSet s e i v env
infer (L.SEReturn _ _ v) env = infer v env
infer (L.SEPrint _ _ _) env = (Right L.Unit', env)
infer (L.SEFunc f) env = inferFunc f env
infer (L.SEFuncCall s e iden args) env = inferFuncCall s e iden args env
infer L.SEComment env = (Right L.String', env)
infer s env = (Left [E.NotImplemented s], env)

inferIdentifier :: L.LizPos -> L.LizPos -> T.Text -> Env -> (Either [E.SemErr] L.Type, Env)
inferIdentifier s e iden env@(Env {envFuncs=fenv, envVars=venv, envConsts=cenv}) =
  let
    func = iden `M.member` fenv
    var = iden `M.member` venv
    constant = iden `M.member` cenv
  in aux s e iden func var constant
  where
    aux st end i True True _ = (Left $ [E.IdentifierAlreadyInUse st end i], env)
    aux st end i True _ True = (Left $ [E.IdentifierAlreadyInUse st end i], env)
    aux st end i _ True True = (Left $ [E.IdentifierAlreadyInUse st end i], env)
    -- aux _ _ _ (Just L.Func{funcReturnType=ty}) _ _ = (Right ty, env)
    aux _ _ _ True _ _ = let (ty, _) = fenv M.! iden in (Right ty, env)
    -- aux _ _ _ _ (Just L.Var{varType=ty}) _ = (Right ty, env)
    aux _ _ _ _ True _ = let ty = venv M.! iden in (Right ty, env)
    -- aux _ _ _ _ _ (Just L.Var{varType=ty}) = (Right ty, env)
    aux _ _ _ _ _ True = let ty = cenv M.! iden in (Right ty, env)
    aux st end i False False False = (Left $ [E.UndefinedIdentifier st end i], env)

inferUnary :: L.LizPos -> L.LizPos -> L.UnaryOp -> L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
inferUnary s e op v env =
  case (infer v env) of
    err@((Left _), _) -> err
    (Right operandType, t) -> (aux op operandType, t)
  where
    aux :: L.UnaryOp -> L.Type -> Either [E.SemErr] L.Type
    aux L.Negate L.Int' = Right L.Int'
    aux L.Negate L.Float' = Right L.Float'
    aux L.Negate t = Left $ [E.IncorrectTypes s e "Float or Int" [t]]

    aux L.Not L.Bool' = Right L.Bool'
    aux L.Not t = Left $ [E.IncorrectType s e L.Bool' t]

inferBinary :: L.LizPos -> L.LizPos -> L.BinaryOp -> L.SExpr -> L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
inferBinary s e op l r env =
  case (infer l env, infer r env) of
    (err@(Left _, _), _) -> err
    (_, err@(Left _, _)) -> err
    ((Right leftType, nenv), (Right rightType, _)) -> (aux op leftType rightType, nenv) -- can't define stuff in binary sexprs
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
      | otherwise = Left $ [E.IncorrectType s e lt rt]

inferVariable :: L.LizPos -> L.LizPos -> L.Var -> Env -> Bool -> (Either [E.SemErr] L.Type, Env)
inferVariable s e L.Var{..} env isConst =
  case (infer varValue env) of
    err@(Left _, _) -> err
    (Right ty, nenv) -> aux varIdent varType ty nenv
  where
    aux :: T.Text -> L.Type -> L.Type -> Env -> (Either [E.SemErr] L.Type, Env)
    aux ident decType valType t@(Env {envVars=varMap, envConsts=constMap, envFuncs=funcMap})
      | ident `M.member` varMap || ident `M.member` constMap || ident `M.member` funcMap = (Left $ [E.IdentifierAlreadyInUse s e ident], env)
      | valType /= decType && valType /= L.Undef' = 
        let newenv = M.insert ident varType -- so that you don't get a bunch of undefined variable errors.
        in if isConst then (Left $ [E.IncorrectType s e decType valType], t{envConsts=newenv constMap})
                      else (Left $ [E.IncorrectType s e decType valType], t{envVars=newenv varMap})
      | otherwise = 
        let newenv = M.insert ident valType 
        in if isConst then (Right decType, t{envConsts=newenv constMap})
                      else (Right decType, t{envVars=newenv varMap})

inferSet :: L.LizPos -> L.LizPos -> T.Text -> L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
inferSet s e ident v env =
  case (infer v env) of
    err@(Left _, _) -> err
    (Right ty, nenv) -> aux ident ty nenv
  where
    aux :: T.Text -> L.Type -> Env -> (Either [E.SemErr] L.Type, Env)
    aux i ty table@(Env{..})
      | i `M.member` envConsts = (Left $ [E.AssigningToConstant s e i], table)
      | i `M.member` envFuncs = (Left $ [E.AssigningToFunction s e ident], table)
      | otherwise =
        let value = i `M.lookup` envVars in 
        case value of
          Nothing -> (Left $ [E.UndefinedIdentifier s e i], table)
          Just correctType | correctType == ty -> (Right ty, table)
                           | otherwise -> (Left $ [E.IncorrectType s e correctType ty], table)

inferFunc :: L.Func -> Env -> (Either [E.SemErr] L.Type, Env)
inferFunc (L.Func{..}) env@(Env{..})
  | funcIdent `M.member` envFuncs || funcIdent `M.member` envVars || funcIdent `M.member` envConsts = (Left $ [E.IdentifierAlreadyInUse funcStart funcEnd funcIdent], env)
  | otherwise =
    let
      -- envWithArgs = flip addArgs env $ map (\L.Arg{..} -> L.Var{varIdent=argIdent, varType=argType}) funcArgs
      envWithArgs = flip addArgs env $ map (\L.Arg{..} -> (argIdent, argType)) funcArgs
      (result, nenv, vis, cis, fis) = evaluateFuncBody funcBody envWithArgs
      errsAndTypes = collectErrors result [] []
    in aux (map L.argIdent funcArgs) vis cis fis errsAndTypes funcReturnType nenv
  where
    aux argIdents vis cis fis (errs, types) ret table@(Env {envConsts=constMap, envVars=varMap, envFuncs=funcMap}) =
      let
      -- removing anything declared within the function from the table.
          nixConsts = foldl' (flip M.delete) constMap (argIdents ++ cis)
          nixVars = foldl' (flip M.delete) varMap vis
          nixFuncs = foldl' (flip M.delete) funcMap fis
          nixFuncDefsenv = table{envConsts=nixConsts, envVars=nixVars, envFuncs=nixFuncs}
      in 
      case () of _
                  | last types /= ret -> (Left $ [E.IncorrectType funcStart funcEnd ret (last types)], nixFuncDefsenv)
                  | length errs /= 0 -> (Left errs, nixFuncDefsenv)
                  | otherwise -> 
                      let newFuncMap = M.insert funcIdent (funcReturnType, (map L.argType funcArgs)) (nixFuncs)
                      in (Right ret, nixFuncDefsenv{envFuncs=newFuncMap})

    addArgs :: [(T.Text, L.Type)] -> Env -> Env
    addArgs [] table = table
    addArgs ((i, t) : xs) e@(Env{envConsts=cenv}) = addArgs xs $ e{envConsts=M.insert i t cenv}-- args are constant by default

    evaluateFuncBody :: [L.SExpr] -> Env -> ([Either [E.SemErr] L.Type], Env, [T.Text], [T.Text], [T.Text])
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

inferFuncCall :: L.LizPos -> L.LizPos -> T.Text -> [L.SExpr] -> Env -> (Either [E.SemErr] L.Type, Env)
inferFuncCall s e ident sexprs env@(Env{..})
  | ident `M.notMember` envFuncs || ident `M.member` envConsts || ident `M.member` envVars = (Left $ [E.UndefinedIdentifier s e ident], env)
  | otherwise =
    case (ident `M.lookup` envFuncs) of
        Nothing -> (Left $ [E.UndefinedIdentifier s e ident], env)
        Just (ty, argTys) ->
          let 
            evaluated_sexprs = map (fst . flip infer env) sexprs 
            (errs, types) = collectErrors evaluated_sexprs [] []
          in if length errs /= 0 then (Left errs, env)
                                 else aux types argTys ty
  where
    aux :: [L.Type] -> [L.Type] -> L.Type -> (Either [E.SemErr] L.Type, Env)
    aux ts as ret 
      | length ts > length as = (Left $ [E.TooManyArgs s e ident (length ts - length as)], env)
      | length ts < length as = (Left $ [E.NotEnoughArgs s e ident (length as - length ts)], env) 
      | otherwise =
        let wts = findWrongTypes ts as in
        if length wts /= 0 then (Left $ [E.IncorrectArgTypes s e ident as wts], env)
                           else (Right ret, env)

    findWrongTypes :: [L.Type] -> [L.Type] -> [L.Type]
    findWrongTypes [] _ = []
    findWrongTypes (x : xs) ys | not $ x `elem` ys = x : findWrongTypes xs ys
                               | otherwise = findWrongTypes xs ys
