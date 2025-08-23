{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema.Typecheck where

import qualified Liz.Common.Errors as E
import qualified Liz.Common.Logging as Log
import qualified Liz.Common.Types as CT

import Liz.Sema.Macro
import Liz.Sema.Terminators

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Data.List (mapAccumL)
import Data.Either (lefts, rights)
import Data.Foldable (fold)

data Env = Env 
  { envFuncs  :: M.Map T.Text (CT.Type, [CT.Type])
  , envVars   :: M.Map T.Text CT.Type
  , envConsts :: M.Map T.Text CT.Type
  } deriving (Show, Eq)

-- helper sema functions
mkEnv :: Env
mkEnv = Env {envFuncs = M.empty, envVars = M.empty, envConsts = M.empty}

inferBody :: [CT.SExpr] -> Env -> [Either [E.SemErr] CT.Type]
inferBody exprs env = 
  reverse . snd . fst $ mapAccumL 
    (\(env', acc) expr -> 
      let (expr', env'') = infer expr env' in 
      ((env'', expr' : acc), env'')) (env, []) (reverse exprs)

analyseAndPrintErrs :: CT.Program -> FilePath -> String -> IO ()
analyseAndPrintErrs prog f ftext =
  let 
    subbed_prog = substituteMacros prog 
    errs = fold $ lefts [subbed_prog]
  in
  if length errs /= 0 
  then Log.printErrs f ftext errs []
  else
    let
      (CT.Program funcs' glbls' _) = head' $ rights [subbed_prog]
      (glbl_errs, env') = inferGlbls glbls' mkEnv []
      (func_errs, mainCount) = inferFuncs funcs' env' 0 []
    in
    case () of _
                | length glbl_errs /= 0 && length func_errs /= 0 && mainCount > 1 -> Log.printErrs f ftext ((E.MultipleEntrypoints : glbl_errs) <> func_errs) []
                | length glbl_errs /= 0 && length func_errs /= 0 && mainCount == 0 -> Log.printErrs f ftext (E.NoEntrypoint : func_errs) []
                | length glbl_errs /= 0 && mainCount > 1 -> Log.printErrs f ftext (E.MultipleEntrypoints : glbl_errs) []
                | length glbl_errs /= 0 && mainCount == 0 -> Log.printErrs f ftext (E.NoEntrypoint : glbl_errs) []
                | length func_errs /= 0 && mainCount > 1 -> Log.printErrs f ftext (E.MultipleEntrypoints : func_errs) []
                | length func_errs /= 0 && mainCount == 0 -> Log.printErrs f ftext (E.NoEntrypoint : func_errs) []
                | length glbl_errs /= 0 -> Log.printErrs f ftext glbl_errs []
                | length func_errs /= 0 -> Log.printErrs f ftext func_errs []
                | mainCount > 1 -> Log.printErrs f ftext [E.MultipleEntrypoints] []
                | mainCount == 0 -> Log.printErrs f ftext [E.NoEntrypoint] []
                | otherwise -> putStrLn "all good"
  where
    inferGlbls :: [CT.GlblVar] -> Env -> [Either [E.SemErr] CT.Type] -> ([E.SemErr], Env)
    inferGlbls [] env acc = (fold $ lefts acc, env)
    inferGlbls ((CT.GlblVar range v) : gs) env acc =
      let (result, env') = inferVariable range v env True in
      inferGlbls gs env' (result : acc)

    inferFuncs :: [CT.Func] -> Env -> Int -> [Either [E.SemErr] CT.Type] -> ([E.SemErr], Int)
    inferFuncs [] _ i acc = (fold $ lefts acc, i)
    inferFuncs (fn@CT.Func{funcIdent=ident} : fs) env i acc =
      let (result, env') = inferFunc fn env in 
      if ident == "main" then inferFuncs fs env' (i + 1) (result : acc)
                         else inferFuncs fs env' i (result : acc)

-- TODO: Allow declarations in any order.
analyseProgram :: CT.Program -> Either [E.SemErr] CT.Program
analyseProgram prog = do
  prog' <- inspectTerminators prog
  subbed_prog@(CT.Program funcs' glbls' _) <- substituteMacros prog'
  let 
    (glbl_errs, env') = inferGlbls glbls' mkEnv []
    (func_errs, mainCount) = inferFuncs funcs' env' 0 []
  case () of _
  -- TODO: do something about this
              | length glbl_errs /= 0 && length func_errs /= 0 && mainCount > 1 -> Left $ (E.MultipleEntrypoints : glbl_errs) <> func_errs
              | length glbl_errs /= 0 && length func_errs /= 0 && mainCount == 0 -> Left $ E.NoEntrypoint : func_errs
              | length glbl_errs /= 0 && mainCount > 1 -> Left $ E.MultipleEntrypoints : glbl_errs
              | length glbl_errs /= 0 && mainCount == 0 -> Left $ E.NoEntrypoint : glbl_errs
              | length func_errs /= 0 && mainCount > 1 -> Left $ E.MultipleEntrypoints : func_errs
              | length func_errs /= 0 && mainCount == 0 -> Left $ E.NoEntrypoint : func_errs
              | length glbl_errs /= 0 -> Left glbl_errs
              | length func_errs /= 0 -> Left func_errs
              | mainCount > 1 -> Left [E.MultipleEntrypoints]
              | mainCount == 0 -> Left [E.NoEntrypoint]
              | otherwise -> Right subbed_prog
  where
    inferGlbls :: [CT.GlblVar] -> Env -> [Either [E.SemErr] CT.Type] -> ([E.SemErr], Env)
    inferGlbls [] env acc = (fold $ lefts acc, env)
    inferGlbls ((CT.GlblVar range v) : gs) env acc =
      let (result, env') = inferVariable range v env True in
      inferGlbls gs env' (result : acc)

    inferFuncs :: [CT.Func] -> Env -> Int -> [Either [E.SemErr] CT.Type] -> ([E.SemErr], Int)
    inferFuncs [] _ i acc = (fold $ lefts acc, i)
    inferFuncs (f@CT.Func{funcIdent=ident} : fs) env i acc =
      let (result, env') = inferFunc f env in
      if ident == "main" then inferFuncs fs env' (i + 1) (result : acc)
                         else inferFuncs fs env' i (result : acc)

-- main typechecking/sema functions.
infer :: CT.SExpr -> Env -> (Either [E.SemErr] CT.Type, Env)
infer (CT.SEExpr ex) env = inferExpr ex env
infer (CT.SEFlow (CT.FBlockStmt _ body)) env = inferBlock (NE.toList body) env
infer (CT.SEFlow (CT.FIfStmt range cond tbr fbr)) env = inferIfStmt range cond tbr fbr env
infer (CT.SEVar range v) env = inferVariable range v env False
infer (CT.SEConst range v) env = inferVariable range v env True
infer (CT.SESet range i v) env = inferSet range i v env
infer CT.SEComment env = (Right CT.String', env)
infer s env = (Left [E.NotImplemented s], env)

inferExpr :: CT.Expression -> Env -> (Either [E.SemErr] CT.Type, Env)
inferExpr (CT.ELiteral ty _ _) env = (Right ty, env)
inferExpr (CT.EUnary op range v) env = checkUnary range op v env
inferExpr (CT.EBinary op range l r) env = checkBinary range op l r env
inferExpr (CT.EReturn _ v) env = inferExpr v env
inferExpr (CT.EPrint range e) env = 
  case (inferExpr e env) of 
    (Right CT.String', _) -> (Right CT.Unit', env)
    (Right t, _) -> (Left [E.IncorrectType range CT.String' t], env)
    (err@(Left _), _) -> (err, env)
inferExpr (CT.EFuncCall range iden args) env = inferFuncCall range iden args env
inferExpr (CT.EIdentifier iden range) env = inferIdentifier range iden env
inferExpr (CT.EFormat _ _ _) env = (Right CT.String', env)

inferIdentifier :: CT.LizRange -> T.Text -> Env -> (Either [E.SemErr] CT.Type, Env)
inferIdentifier range iden env@(Env {envFuncs=fenv, envVars=venv, envConsts=cenv}) =
  let
    func = iden `M.member` fenv
    var = iden `M.member` venv
    constant = iden `M.member` cenv
  in aux range iden func var constant
  where
    aux r i True True _ = (Left $ [E.IdentifierAlreadyInUse r i], env)
    aux r i True _ True = (Left $ [E.IdentifierAlreadyInUse r i], env)
    aux r i _ True True = (Left $ [E.IdentifierAlreadyInUse r i], env)
    aux _ _ True _ _ = let (ty, _) = fenv M.! iden in (Right ty, env)
    aux _ _ _ True _ = let ty = venv M.! iden in (Right ty, env)
    aux _ _ _ _ True = let ty = cenv M.! iden in (Right ty, env)
    aux r i False False False = (Left $ [E.UndefinedIdentifier r i], env)

checkUnary :: CT.LizRange -> CT.UnaryOp -> CT.Expression -> Env -> (Either [E.SemErr] CT.Type, Env)
checkUnary range _ (CT.EReturn _ _) env = (Left [E.InvalidUnaryExpr range], env)
checkUnary range op v env =
  case (inferExpr v env) of
    err@((Left _), _) -> err
    (Right operandType, t) -> (aux op operandType, t)
  where
    aux :: CT.UnaryOp -> CT.Type -> Either [E.SemErr] CT.Type
    aux CT.Negate CT.Int' = Right CT.Int'
    aux CT.Negate CT.Float' = Right CT.Float'
    aux CT.Negate t = Left $ [E.IncorrectTypes range "Float or Int" [t]]

    aux CT.Not CT.Bool' = Right CT.Bool'
    aux CT.Not t = Left $ [E.IncorrectType range CT.Bool' t]

checkBinary :: CT.LizRange -> CT.BinaryOp -> CT.Expression -> CT.Expression -> Env -> (Either [E.SemErr] CT.Type, Env)
checkBinary range _ (CT.EReturn _ _) (CT.EReturn _ _) env = (Left [E.InvalidBinaryExpr range], env)
checkBinary range _ _ (CT.EReturn _ _) env = (Left [E.InvalidBinaryExpr range], env)
checkBinary range _ (CT.EReturn _ _) _ env = (Left [E.InvalidBinaryExpr range], env)
checkBinary range op l r env =
  case (inferExpr l env, inferExpr r env) of
    ((Left el, env'), (Left er, _)) -> (Left $ el <> er, env')
    (err@(Left _, _), _) -> err
    (_, err@(Left _, _)) -> err
    ((Right leftType, _), (Right rightType, _)) -> (aux op leftType rightType, env) -- can't define stuff in binary sexprs
  where
    aux :: CT.BinaryOp -> CT.Type -> CT.Type -> Either [E.SemErr] CT.Type
    aux CT.Concat CT.String' CT.String' = Right CT.String'
    aux CT.Concat CT.String' rt = Left $ [E.IncorrectType range CT.String' rt]
    aux CT.Concat lt CT.String' = Left $ [E.IncorrectType range CT.String' lt]
    aux CT.Concat lt rt = Left $ [E.IncorrectTypes range "String"  [lt, rt]]

    aux (CT.Add; CT.Subtract; CT.Multiply; CT.Divide) CT.Int' CT.Int' = Right CT.Int'
    aux (CT.Add; CT.Subtract; CT.Multiply; CT.Divide) lt CT.Int' = Left [E.IncorrectType range CT.Int' lt]
    aux (CT.Add; CT.Subtract; CT.Multiply; CT.Divide) CT.Int' rt = Left [E.IncorrectType range CT.Int' rt]

    aux (CT.Add; CT.Subtract; CT.Multiply; CT.Divide) CT.Float' CT.Float' = Right CT.Float'
    aux (CT.Add; CT.Subtract; CT.Multiply; CT.Divide) lt CT.Float' = Left [E.IncorrectType range CT.Float' lt]
    aux (CT.Add; CT.Subtract; CT.Multiply; CT.Divide) CT.Float' rt = Left [E.IncorrectType range CT.Float' rt]
    aux (CT.Add; CT.Subtract; CT.Multiply; CT.Divide) lt rt = Left $ [E.IncorrectTypes range "Float or Int" [lt, rt]]

    aux (CT.Less; CT.Greater; CT.Eql; CT.NotEql; CT.GreaterEql; CT.LessEql) lt rt
      | lt == rt = Right CT.Bool'
      | otherwise = Left $ [E.IncorrectType range lt rt]

inferVariable :: CT.LizRange -> CT.Var -> Env -> Bool -> (Either [E.SemErr] CT.Type, Env)
inferVariable range CT.Var{..} env isConst =
  case (inferExpr varValue env) of
    err@(Left _, _) -> err
    (Right ty, env') -> aux varIdent varType ty env'
  where
    aux :: T.Text -> CT.Type -> CT.Type -> Env -> (Either [E.SemErr] CT.Type, Env)
    aux ident decType valType t@(Env {envVars=varMap, envConsts=constMap, envFuncs=funcMap})
      | ident `M.member` varMap || ident `M.member` constMap || ident `M.member` funcMap = (Left $ [E.IdentifierAlreadyInUse range ident], env)
      | valType /= decType = 
        let newenv = M.insert ident varType -- so that you don't get a bunch of undefined variable errors.
        in if isConst then (Left $ [E.IncorrectType range decType valType], t{envConsts=newenv constMap})
                      else (Left $ [E.IncorrectType range decType valType], t{envVars=newenv varMap})
      | otherwise = 
        let newenv = M.insert ident decType 
        in if isConst then (Right decType, t{envConsts=newenv constMap})
                      else (Right decType, t{envVars=newenv varMap})

inferSet :: CT.LizRange -> T.Text -> CT.Expression -> Env -> (Either [E.SemErr] CT.Type, Env)
inferSet range ident v env =
  case (inferExpr v env) of
    err@(Left _, _) -> err
    (Right ty, env') -> aux ident ty env'
  where
    aux :: T.Text -> CT.Type -> Env -> (Either [E.SemErr] CT.Type, Env)
    aux i ty e@(Env{..})
      | i `M.member` envConsts = (Left $ [E.AssigningToConstant range i], e)
      | i `M.member` envFuncs = (Left $ [E.AssigningToFunction range ident], e)
      | otherwise =
        let value = i `M.lookup` envVars in 
        case value of
          Nothing -> (Left $ [E.UndefinedIdentifier range i], e)
          Just correctType | correctType == ty -> (Right ty, e)
                           | otherwise -> (Left $ [E.IncorrectType range correctType ty], e)

inferFunc :: CT.Func -> Env -> (Either [E.SemErr] CT.Type, Env)
inferFunc (CT.Func{..}) env@(Env{..})
  | funcIdent `M.member` envFuncs || funcIdent `M.member` envVars || funcIdent `M.member` envConsts = (Left $ [E.IdentifierAlreadyInUse funcPos funcIdent], env)
  | otherwise =
    let
      argTypeErrs = 
        lefts $ foldMap (\CT.Arg{..} -> if argType == CT.Unit' then [Left (E.InvalidArgType funcPos argIdent argType)] else [Right ()]) funcArgs
      envWithArgs = flip addArgs env $ map (\CT.Arg{..} -> (argIdent, argType)) funcArgs
      errsAndTypes = collectErrors $ inferBody funcBody envWithArgs
    in aux errsAndTypes funcReturnType argTypeErrs
  where
    aux (errs, types) ret aterrs =
      case () of _
                  | length aterrs /= 0 && length errs /= 0 && last types /= ret -> (Left $ (E.IncorrectType funcPos ret (last types)) : aterrs <> errs, env)
                  | length errs /= 0 && length aterrs /= 0 -> (Left $ aterrs <> errs, env)
                  -- | length aterrs /= 0 && last types /= ret -> (Left $ [E.IncorrectType funcPos ret (last types)] <> aterrs, env)
                  -- | length errs /= 0 && last types /= ret -> (Left $ [E.IncorrectType funcPos ret (last types)] <> errs, env)
                  | length errs /= 0 -> (Left errs, env)
                  | length aterrs /= 0 -> (Left aterrs, env)
                  | last types /= ret -> (Left $ [E.IncorrectType funcPos ret (last types)], env)
                  | otherwise -> 
                      let newFuncMap = M.insert funcIdent (funcReturnType, (map CT.argType funcArgs)) (envFuncs)
                      in (Right ret, env{envFuncs=newFuncMap})

    addArgs :: [(T.Text, CT.Type)] -> Env -> Env
    addArgs [] e = e
    addArgs ((i, t) : xs) e@(Env{envConsts=cenv}) = addArgs xs $ e{envConsts=M.insert i t cenv} -- args are constant by default

inferBlock :: [CT.SExpr] -> Env -> (Either [E.SemErr] CT.Type, Env)
inferBlock body env =
  let 
    result = inferBody body env
    (errs, types) = collectErrors result
  in
  if length errs /= 0 then (Left errs, env)
                      else (Right $ last types, env)

inferFuncCall :: CT.LizRange -> T.Text -> [CT.Expression] -> Env -> (Either [E.SemErr] CT.Type, Env)
inferFuncCall range ident sexprs env@(Env{..})
  | ident `M.notMember` envFuncs || ident `M.member` envConsts || ident `M.member` envVars = (Left $ [E.UndefinedIdentifier range ident], env)
  | otherwise =
    case (ident `M.lookup` envFuncs) of
        Nothing -> (Left $ [E.UndefinedIdentifier range ident], env)
        Just (ty, argTys) ->
          let 
            evaluated_sexprs = map (fst . flip inferExpr env) sexprs 
            (errs, types) = collectErrors evaluated_sexprs
          in if length errs /= 0 then (Left errs, env)
                                 else aux types argTys ty
  where
    aux :: [CT.Type] -> [CT.Type] -> CT.Type -> (Either [E.SemErr] CT.Type, Env)
    aux ts as ret 
      | length ts > length as = (Left $ [E.TooManyArgs range ident (length ts - length as)], env)
      | length ts < length as = (Left $ [E.NotEnoughArgs range ident (length as - length ts)], env) 
      | otherwise =
        let wts = findWrongTypes ts as in
        if length wts /= 0 then (Left $ [E.IncorrectArgTypes range ident as wts], env)
                           else (Right ret, env)

    findWrongTypes :: [CT.Type] -> [CT.Type] -> [CT.Type]
    findWrongTypes [] _ = []
    findWrongTypes (x : xs) ys | not $ x `elem` ys = x : findWrongTypes xs ys
                               | otherwise = findWrongTypes xs ys

inferIfStmt :: CT.LizRange -> CT.Expression -> CT.SExpr -> Maybe CT.SExpr -> Env -> (Either [E.SemErr] CT.Type, Env)
inferIfStmt range (CT.EReturn _ _) _ _ env = (Left [E.InvalidIfStmt range], env)
inferIfStmt range cond tbranch Nothing env =
  case (inferExpr cond env, infer tbranch env) of
    ((Left ecs, _), (Left ets, env')) -> (Left $ ecs <> ets, env')
    (err@((Left _), _), _) -> err
    (_, err@(Left _, _)) -> err
    ((Right tccond, _), (Right tctbr, env')) | tccond /= CT.Bool' -> (Left [E.IncorrectType range CT.Bool' tccond], env')
                                             | otherwise -> (Right tctbr, env')
inferIfStmt range cond tbranch (Just fbranch) env =
  case (inferExpr cond env, infer tbranch env, infer fbranch env) of
    ((Left ecs, _), (Left ets, _), (Left efs, env')) -> (Left $ ecs <> ets <> efs, env')
    (err@((Left _), _), _, _) -> err
    (_, err@(Left _, _), _) -> err
    (_, _, err@(Left _, _)) -> err
    ((Right tccond, _), (Right tctbr, _), (Right tcfbr, env')) | tccond /= CT.Bool' && tctbr /= tcfbr -> (Left [E.IncorrectType range CT.Bool' tccond, E.IncorrectType range tctbr tcfbr], env')
                                                               | tccond /= CT.Bool' -> (Left [E.IncorrectType range CT.Bool' tccond], env')
                                                               | tctbr /= tcfbr -> (Left [E.IncorrectType range tctbr tcfbr], env')
                                                               | otherwise -> (Right tcfbr, env')
