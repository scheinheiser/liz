{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema.Sema where

import qualified Liz.Common.Errors as E
import qualified Liz.Common.Logging as Log
import qualified Liz.Common.Types as L
import Liz.Sema.Macro

import qualified Data.Map as M
import qualified Data.Text as T

import Data.List (mapAccumL)
import Data.Either (lefts)
import Data.Maybe (catMaybes)
import Data.Foldable (fold)

data Env = Env 
  { envFuncs  :: M.Map T.Text (L.Type, [L.Type])
  , envVars   :: M.Map T.Text L.Type
  , envConsts :: M.Map T.Text L.Type
  } deriving (Show, Eq)

-- helper sema functions
mkEnv :: Env
mkEnv = Env {envFuncs = M.empty, envVars = M.empty, envConsts = M.empty}

inferBody :: [L.SExpr] -> Env -> [Either [E.SemErr] L.Type]
inferBody exprs env = 
  snd . fst $ mapAccumL 
    (\(env', acc) expr -> 
      let (expr', env'') = infer expr env' in 
      ((env'', expr' : acc), env'')) (env, []) exprs

analyseAndPrintErrs :: L.Program -> FilePath -> String -> IO ()
analyseAndPrintErrs (L.Program prog) f ftext = do
  let (prog_errs, subbed_prog) = collectErrors (subBody prog mkMacroTbl) in
    if length prog_errs /= 0 
    then Log.printErrs f ftext (reverse prog_errs) []
    else 
      let
        subbed_prog' = catMaybes subbed_prog
        (res, hasMain) = aux subbed_prog' mkEnv 0 []
        errs = fold $ lefts res
      in
      case () of _
                  | length errs /= 0 && hasMain > 1 -> Log.printErrs f ftext (E.MultipleEntrypoints : errs) []
                  | length errs /= 0 && hasMain == 0 -> Log.printErrs f ftext (E.NoEntrypoint : errs) []
                  | length errs /= 0 -> Log.printErrs f ftext errs []
                  | hasMain == 0 -> Log.printErrs f ftext [E.NoEntrypoint] []
                  | hasMain > 1 -> Log.printErrs f ftext [E.MultipleEntrypoints] []
                  | otherwise -> putStrLn "all good"
  where
    aux :: [L.SExpr] -> Env -> Int -> [Either [E.SemErr] L.Type] -> ([Either [E.SemErr] L.Type], Int)
    aux [] _ hasMain acc = (acc, hasMain)
    aux (ex@(L.SEFlow (L.FFunc L.Func{funcIdent=i})) : exprs) sym hasMain acc =
      let
        (res, next) = infer ex sym
      in if i == "main" then aux exprs next (hasMain + 1) (res : acc)
                        else aux exprs next hasMain (res : acc)
    aux (ex : exprs) sym hasMain acc =
      let
        (res, next) = infer ex sym
      in aux exprs next hasMain (res : acc)

-- TODO: Allow declarations in any order.
analyseProgram :: L.Program -> Either [E.SemErr] L.Program
analyseProgram (L.Program prog) = 
  let (prog_errs, subbed_prog) = collectErrors (subBody prog mkMacroTbl) in
  if length prog_errs /= 0 
  then Left prog_errs
  else
    let
      subbed_prog' = catMaybes (reverse subbed_prog)
      (res, hasMain) = aux subbed_prog' mkEnv 0 []
      errs = fold $ lefts res
    in
    case () of _
                | length errs /= 0 && hasMain > 1 -> Left $ E.MultipleEntrypoints : errs
                | length errs /= 0 && hasMain == 0 -> Left $ E.NoEntrypoint : errs
                | length errs /= 0 -> Left errs
                | hasMain == 0 -> Left [E.NoEntrypoint]
                | hasMain > 1 -> Left [E.MultipleEntrypoints]
                | otherwise -> Right $ L.Program subbed_prog'
  where
    aux :: [L.SExpr] -> Env -> Int -> [Either [E.SemErr] L.Type] -> ([Either [E.SemErr] L.Type], Int)
    aux [] _ hasMain acc = (acc, hasMain)
    aux (ex@(L.SEFlow (L.FFunc L.Func{funcIdent=i})) : exprs) env hasMain acc =
      let
        (res, env') = infer ex env
      in if i == "main" then aux exprs env' (hasMain + 1) (res : acc)
                        else aux exprs env' hasMain (res : acc)
    aux (ex : exprs) env hasMain acc =
      let
        (res, env') = infer ex env
      in aux exprs env' hasMain (res : acc)


-- main typechecking/sema functions.
infer :: L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
infer (L.SEExpr ex) env = inferExpr ex env
infer (L.SEFlow (L.FFunc f)) env = inferFunc f env
infer (L.SEFlow (L.FBlockStmt _ body)) env = inferBlock body env
infer (L.SEFlow (L.FIfStmt range cond tbr fbr)) env = inferIfStmt range cond tbr fbr env
infer (L.SEVar range v) env = inferVariable range v env False
infer (L.SEConst range v) env = inferVariable range v env True
infer (L.SESet range i v) env = inferSet range i v env
infer L.SEComment env = (Right L.String', env)
infer s env = (Left [E.NotImplemented s], env)

inferExpr :: L.Expression -> Env -> (Either [E.SemErr] L.Type, Env)
inferExpr (L.ELiteral ty _ _) env = (Right ty, env)
inferExpr (L.EUnary op range v) env = checkUnary range op v env
inferExpr (L.EBinary op range l r) env = checkBinary range op l r env
inferExpr (L.EReturn _ v) env = inferExpr v env
inferExpr (L.EPrint _ _) env = (Right L.Unit', env)
inferExpr (L.EFuncCall range iden args) env = inferFuncCall range iden args env
inferExpr (L.EIdentifier iden range) env = inferIdentifier range iden env

inferIdentifier :: L.LizRange -> T.Text -> Env -> (Either [E.SemErr] L.Type, Env)
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

checkUnary :: L.LizRange -> L.UnaryOp -> L.Expression -> Env -> (Either [E.SemErr] L.Type, Env)
checkUnary range op v env =
  case (inferExpr v env) of
    err@((Left _), _) -> err
    (Right operandType, t) -> (aux op operandType, t)
  where
    aux :: L.UnaryOp -> L.Type -> Either [E.SemErr] L.Type
    aux L.Negate L.Int' = Right L.Int'
    aux L.Negate L.Float' = Right L.Float'
    aux L.Negate t = Left $ [E.IncorrectTypes range "Float or Int" [t]]

    aux L.Not L.Bool' = Right L.Bool'
    aux L.Not t = Left $ [E.IncorrectType range L.Bool' t]

checkBinary :: L.LizRange -> L.BinaryOp -> L.Expression -> L.Expression -> Env -> (Either [E.SemErr] L.Type, Env)
checkBinary range op l r env =
  case (inferExpr l env, inferExpr r env) of
    ((Left el, env'), (Left er, _)) -> (Left $ el <> er, env')
    (err@(Left _, _), _) -> err
    (_, err@(Left _, _)) -> err
    ((Right leftType, _), (Right rightType, _)) -> (aux op leftType rightType, env) -- can't define stuff in binary sexprs
  where
    aux :: L.BinaryOp -> L.Type -> L.Type -> Either [E.SemErr] L.Type
    aux L.Concat L.String' L.String' = Right L.String'
    aux L.Concat L.String' rt = Left $ [E.IncorrectType range L.String' rt]
    aux L.Concat lt L.String' = Left $ [E.IncorrectType range L.String' lt]
    aux L.Concat lt rt = Left $ [E.IncorrectTypes range "String"  [lt, rt]]

    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Int' L.Int' = Right L.Int'
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) lt L.Int' = Left [E.IncorrectType range L.Int' lt]
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Int' rt = Left [E.IncorrectType range L.Int' rt]

    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Float' L.Float' = Right L.Float'
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) lt L.Float' = Left [E.IncorrectType range L.Float' lt]
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) L.Float' rt = Left [E.IncorrectType range L.Float' rt]
    aux (L.Add; L.Subtract; L.Multiply; L.Divide) lt rt = Left $ [E.IncorrectTypes range "Float or Int" [lt, rt]]

    aux (L.Less; L.Greater; L.Eql; L.NotEql; L.GreaterEql; L.LessEql) lt rt
      | lt == rt = Right L.Bool'
      | otherwise = Left $ [E.IncorrectType range lt rt]

inferVariable :: L.LizRange -> L.Var -> Env -> Bool -> (Either [E.SemErr] L.Type, Env)
inferVariable range L.Var{..} env isConst =
  case (inferExpr varValue env) of
    err@(Left _, _) -> err
    (Right ty, env') -> aux varIdent varType ty env'
  where
    aux :: T.Text -> L.Type -> L.Type -> Env -> (Either [E.SemErr] L.Type, Env)
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

inferSet :: L.LizRange -> T.Text -> L.Expression -> Env -> (Either [E.SemErr] L.Type, Env)
inferSet range ident v env =
  case (inferExpr v env) of
    err@(Left _, _) -> err
    (Right ty, env') -> aux ident ty env'
  where
    aux :: T.Text -> L.Type -> Env -> (Either [E.SemErr] L.Type, Env)
    aux i ty e@(Env{..})
      | i `M.member` envConsts = (Left $ [E.AssigningToConstant range i], e)
      | i `M.member` envFuncs = (Left $ [E.AssigningToFunction range ident], e)
      | otherwise =
        let value = i `M.lookup` envVars in 
        case value of
          Nothing -> (Left $ [E.UndefinedIdentifier range i], e)
          Just correctType | correctType == ty -> (Right ty, e)
                           | otherwise -> (Left $ [E.IncorrectType range correctType ty], e)

inferFunc :: L.Func -> Env -> (Either [E.SemErr] L.Type, Env)
inferFunc (L.Func{..}) env@(Env{..})
  | funcIdent `M.member` envFuncs || funcIdent `M.member` envVars || funcIdent `M.member` envConsts = (Left $ [E.IdentifierAlreadyInUse funcPos funcIdent], env)
  | otherwise =
    let
      argTypeErrs = 
        lefts $ foldMap (\L.Arg{..} -> if argType == L.Unit' then [Left (E.InvalidArgType funcPos argIdent argType)] else [Right ()]) funcArgs
      envWithArgs = flip addArgs env $ map (\L.Arg{..} -> (argIdent, argType)) funcArgs
      result = inferBody funcBody envWithArgs
      errsAndTypes = collectErrors result
    in aux errsAndTypes funcReturnType argTypeErrs
  where
    aux (errs, types) ret aterrs =
      case () of _
                  | length aterrs /= 0 && length errs /= 0 && last types /= ret -> (Left $ (E.IncorrectType funcPos ret (last types)) : aterrs <> errs, env)
                  | length errs /= 0 && length aterrs /= 0 -> (Left $ aterrs <> errs, env)
                  | last types /= ret && length aterrs /= 0 -> (Left $ [E.IncorrectType funcPos ret (last types)] <> aterrs, env)
                  | last types /= ret && length errs /= 0 -> (Left $ [E.IncorrectType funcPos ret (last types)] <> errs, env)
                  | length errs /= 0 -> (Left errs, env)
                  | length aterrs /= 0 -> (Left aterrs, env)
                  | last types /= ret -> (Left $ [E.IncorrectType funcPos ret (last types)], env)
                  | otherwise -> 
                      let newFuncMap = M.insert funcIdent (funcReturnType, (map L.argType funcArgs)) (envFuncs)
                      in (Right ret, env{envFuncs=newFuncMap})

    addArgs :: [(T.Text, L.Type)] -> Env -> Env
    addArgs [] e = e
    addArgs ((i, t) : xs) e@(Env{envConsts=cenv}) = addArgs xs $ e{envConsts=M.insert i t cenv} -- args are constant by default

inferBlock :: [L.SExpr] -> Env -> (Either [E.SemErr] L.Type, Env)
inferBlock body env =
  let 
    result = inferBody body env
    (errs, types) = collectErrors result
  in
  if length errs /= 0 then (Left errs, env)
                      else (Right $ last types, env)

inferFuncCall :: L.LizRange -> T.Text -> [L.Expression] -> Env -> (Either [E.SemErr] L.Type, Env)
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
    aux :: [L.Type] -> [L.Type] -> L.Type -> (Either [E.SemErr] L.Type, Env)
    aux ts as ret 
      | length ts > length as = (Left $ [E.TooManyArgs range ident (length ts - length as)], env)
      | length ts < length as = (Left $ [E.NotEnoughArgs range ident (length as - length ts)], env) 
      | otherwise =
        let wts = findWrongTypes ts as in
        if length wts /= 0 then (Left $ [E.IncorrectArgTypes range ident as wts], env)
                           else (Right ret, env)

    findWrongTypes :: [L.Type] -> [L.Type] -> [L.Type]
    findWrongTypes [] _ = []
    findWrongTypes (x : xs) ys | not $ x `elem` ys = x : findWrongTypes xs ys
                               | otherwise = findWrongTypes xs ys

inferIfStmt :: L.LizRange -> L.Expression -> L.SExpr -> Maybe L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
inferIfStmt range cond tbranch Nothing env =
  case (inferExpr cond env, infer tbranch env) of
    ((Left ecs, _), (Left ets, env')) -> (Left $ ecs <> ets, env')
    (err@((Left _), _), _) -> err
    (_, err@(Left _, _)) -> err
    ((Right tccond, _), (Right tctbr, env')) | tccond /= L.Bool' -> (Left [E.IncorrectType range L.Bool' tccond], env')
                                             | otherwise -> (Right tctbr, env')
inferIfStmt range cond tbranch (Just fbranch) env =
  case (inferExpr cond env, infer tbranch env, infer fbranch env) of
    ((Left ecs, _), (Left ets, _), (Left efs, env')) -> (Left $ ecs <> ets <> efs, env')
    (err@((Left _), _), _, _) -> err
    (_, err@(Left _, _), _) -> err
    (_, _, err@(Left _, _)) -> err
    ((Right tccond, _), (Right tctbr, _), (Right tcfbr, env')) | tccond /= L.Bool' && tctbr /= tcfbr -> (Left [E.IncorrectType range L.Bool' tccond, E.IncorrectType range tctbr tcfbr], env')
                                                               | tccond /= L.Bool' -> (Left [E.IncorrectType range L.Bool' tccond], env')
                                                               | tctbr /= tcfbr -> (Left [E.IncorrectType range tctbr tcfbr], env')
                                                               | otherwise -> (Right tcfbr, env')
