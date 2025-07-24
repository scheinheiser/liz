{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Sema where

import qualified Liz.Common.Errors as E
import qualified Liz.Common.Logging as Log
import qualified Liz.Common.Types as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Data.Either (lefts)
import Data.Maybe (catMaybes)
import Data.Foldable (fold)

data Env = Env 
  { envFuncs  :: M.Map T.Text (L.Type, [L.Type])
  , envVars   :: M.Map T.Text L.Type
  , envConsts :: M.Map T.Text L.Type
  } deriving (Show, Eq)

type MacroTbl = M.Map T.Text L.SExpr

-- helper sema functions
mkEnv :: Env
mkEnv = Env {envFuncs = M.empty, envVars = M.empty, envConsts = M.empty}

inferBody :: [L.SExpr] -> Env -> [Either [E.SemErr] L.Type]
inferBody exprs e = reverse $ aux exprs e
  where
    aux :: [L.SExpr] -> Env -> [Either [E.SemErr] L.Type]
    aux [] _ = []
    aux (x : xs) env = let (res, env') = infer x env in res : aux xs env'

collectErrors :: [Either [E.SemErr] a] -> [E.SemErr] -> [a] -> ([E.SemErr], [a])
collectErrors [] errs others = (errs, others)
collectErrors (x : xs) errs others =
  case x of
    Left e -> collectErrors xs (e <> errs) others
    Right t -> collectErrors xs errs (t : others)

-- helper macro functions
mkMacroTbl :: MacroTbl
mkMacroTbl = M.empty

head' :: [a] -> a
head' = NE.head . NE.fromList

-- if there's a 'none' value, then it must be a macro definition.
hasMacroDef :: [Maybe L.SExpr] -> Bool
hasMacroDef = (0 /=) . length . filter (Nothing ==)

subBody :: [L.SExpr] -> MacroTbl -> [Either [E.SemErr] (Maybe L.SExpr)]
subBody exprs t = reverse $ aux exprs t
  where
    aux :: [L.SExpr] -> MacroTbl -> [Either [E.SemErr] (Maybe L.SExpr)]
    aux [] _ = []
    aux (x : xs) tbl = let (res, tbl') = macroSub x tbl in res : aux xs tbl'

simpleSub :: (L.SExpr -> L.SExpr) -> (L.LizPos, L.LizPos) -> L.SExpr -> MacroTbl -> (Either [E.SemErr] (Maybe L.SExpr), MacroTbl)
simpleSub dec (s, e) v tbl =
  let 
    (subbed_value, _) = macroSub v tbl 
    (value_errs, subbed_value') = collectErrors [subbed_value] [] []
  in
  case () of _
              | length value_errs /= 0 -> (Left value_errs, tbl)
              | hasMacroDef subbed_value' -> (Left [E.NonGlblMacroDef s e], tbl)
              | otherwise ->
                let filtered_value = head' $ catMaybes subbed_value' in
                (Right (Just $ dec filtered_value), tbl)

multiSub :: ([L.SExpr] -> L.SExpr) -> (L.LizPos, L.LizPos) -> [L.SExpr] -> MacroTbl -> (Either [E.SemErr] (Maybe L.SExpr), MacroTbl)
multiSub dec (s, e) body tbl =
  let 
    subbed_body = subBody body tbl 
    (errs, subbed_body')  = collectErrors subbed_body [] []
  in
  case () of _
              | length errs /= 0 -> (Left errs, tbl)
              | hasMacroDef subbed_body' -> (Left [E.NonGlblMacroDef s e], tbl)
              | otherwise ->
                let filtered_body = dec $ catMaybes subbed_body' in
                (Right $ Just filtered_body, tbl)

analyseAndPrintErrs :: L.Program -> FilePath -> IO ()
analyseAndPrintErrs (L.Program prog) f = do
  let (prog_errs, subbed_prog) = collectErrors (subBody prog mkMacroTbl) [] [] in
    if length prog_errs /= 0 
    then Log.printErrs f prog_errs []
    else 
      let
        subbed_prog' = catMaybes subbed_prog
        (res, hasMain) = aux subbed_prog' mkEnv 0 []
        errs = fold $ lefts res
      in
      case () of _
                  | length errs /= 0 && hasMain > 1 -> Log.printErrs f (E.MultipleEntrypoints : errs) []
                  | length errs /= 0 && hasMain == 0 -> Log.printErrs f (E.NoEntrypoint : errs) []
                  | length errs /= 0 -> Log.printErrs f errs []
                  | hasMain == 0 -> Log.printErrs f [E.NoEntrypoint] []
                  | hasMain > 1 -> Log.printErrs f [E.MultipleEntrypoints] []
                  | otherwise -> putStrLn "all good"
  where
    aux :: [L.SExpr] -> Env -> Int -> [Either [E.SemErr] L.Type] -> ([Either [E.SemErr] L.Type], Int)
    aux [] _ hasMain acc = (acc, hasMain)
    aux (ex@(L.SEFunc L.Func{funcIdent=i}) : exprs) sym hasMain acc =
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
  let (prog_errs, subbed_prog) = collectErrors (subBody prog mkMacroTbl) [] [] in
  if length prog_errs /= 0 
  then Left prog_errs
  else
    let
      subbed_prog' = catMaybes subbed_prog
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
    aux (ex@(L.SEFunc L.Func{funcIdent=i}) : exprs) env hasMain acc =
      let
        (res, env') = infer ex env
      in if i == "main" then aux exprs env' (hasMain + 1) (res : acc)
                        else aux exprs env' hasMain (res : acc)
    aux (ex : exprs) env hasMain acc =
      let
        (res, env') = infer ex env
      in aux exprs env' hasMain (res : acc)

-- main macro sub function
macroSub :: L.SExpr -> MacroTbl -> (Either [E.SemErr] (Maybe L.SExpr), MacroTbl)
macroSub (L.SEMacroDef (L.Macro s e i expr)) tbl
            | i `M.member` tbl = (Left [E.IdentifierAlreadyInUse s e i], tbl)
            -- to stop knock-on undefined macro errors. since it just inserts the sexpr, there's no actual issue with infinite in the table.
            | checkRecursiveDef expr i = let newMTbl = M.insert i expr tbl in (Left [E.RecursiveMacroDef s e i], newMTbl)
            | otherwise = let newMTbl = M.insert i expr tbl in (Right Nothing, newMTbl)
  where
    checkRecursiveDef :: L.SExpr -> T.Text -> Bool
    checkRecursiveDef (L.SEValueMacro call_ident _ _) def_ident = call_ident == def_ident
    checkRecursiveDef (L.SEFunc (L.Func _ _ _ _ _ exprs)) def_ident = 
      any (== True) (map (flip checkRecursiveDef def_ident) exprs)
    checkRecursiveDef (L.SEBlockStmt _ _ exprs) def_ident = 
      any (== True) (map (flip checkRecursiveDef def_ident) exprs)
    checkRecursiveDef (L.SEFuncCall _ _ _ params) def_ident = any (== True) (map (flip checkRecursiveDef def_ident) params)
    checkRecursiveDef (L.SEReturn _ _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.SEPrint _ _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.SEBinary _ _ _ l r) def_ident = (checkRecursiveDef l def_ident) || (checkRecursiveDef r def_ident)
    checkRecursiveDef (L.SEUnary _ _ _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.SEVar _ _ (L.Var _ _ v)) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.SEConst _ _ (L.Var _ _ v)) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef (L.SESet _ _ _ v) def_ident = checkRecursiveDef v def_ident
    checkRecursiveDef _ _ = False
macroSub (L.SEValueMacro i s e) tbl =
  let value = i `M.lookup` tbl in
  case value of
    Nothing -> (Left [E.UndefinedIdentifier s e i], tbl)
    Just expr -> (Right (Just expr), tbl)
macroSub (L.SEFunc f@(L.Func _ s e _ _ body)) tbl = multiSub (\x -> L.SEFunc $ f{L.funcBody = x}) (s, e) body tbl
macroSub (L.SEBlockStmt s e body) tbl = multiSub (L.SEBlockStmt s e) (s, e) body tbl
macroSub (L.SEIfStmt s e cond branch Nothing) tbl =
  let
    (subbed_cond, _) = macroSub cond tbl
    (subbed_branch, _) = macroSub branch tbl
    (cond_err, subbed_cond') = collectErrors [subbed_cond] [] []
    (branch_err, subbed_branch') = collectErrors [subbed_branch] [] []
  in
  case () of _
              | length cond_err /= 0 && length branch_err /= 0 -> (Left $ cond_err <> branch_err, tbl)
              | length cond_err /= 0 -> (Left cond_err, tbl)
              | length branch_err /= 0 -> (Left branch_err, tbl)
              | otherwise ->
                let
                  filtered_cond = head' $ catMaybes subbed_cond'
                  filtered_branch = head' $ catMaybes subbed_branch'
                  expr = L.SEIfStmt s e filtered_cond filtered_branch Nothing
                in (Right $ Just expr, tbl)
macroSub (L.SEIfStmt s e cond tbranch (Just fbranch)) tbl =
  let
    (subbed_cond, _) = macroSub cond tbl
    (subbed_tbranch, _) = macroSub tbranch tbl
    (subbed_fbranch, _) = macroSub fbranch tbl
    (cond_err, subbed_cond') = collectErrors [subbed_cond] [] []
    (tbranch_err, subbed_tbranch') = collectErrors [subbed_tbranch] [] []
    (fbranch_err, subbed_fbranch') = collectErrors [subbed_fbranch] [] []
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
                  || hasMacroDef subbed_fbranch' -> (Left [E.NonGlblMacroDef s e], tbl)
              | otherwise ->
                let
                  filtered_cond = head' $ catMaybes subbed_cond'
                  filtered_tbranch = head' $ catMaybes subbed_tbranch'
                  filtered_fbranch = head' $ catMaybes subbed_fbranch'
                  expr = L.SEIfStmt s e filtered_cond filtered_tbranch (Just filtered_fbranch)
                in (Right $ Just expr, tbl)
macroSub (L.SEFuncCall s e i params) tbl = multiSub (L.SEFuncCall s e i) (s, e) params tbl
macroSub (L.SEPrint s e v) tbl = simpleSub (L.SEPrint s e) (s, e) v tbl
macroSub (L.SEReturn s e v) tbl = simpleSub (L.SEReturn s e) (s, e) v tbl
macroSub (L.SEBinary op s e l r) tbl =
  let
    (subbed_left, _) = macroSub l tbl
    (subbed_right, _) = macroSub r tbl
    (left_err, subbed_left') = collectErrors [subbed_left] [] []
    (right_err, subbed_right') = collectErrors [subbed_right] [] []
  in
  case () of _
              | length left_err /= 0 
                && length right_err /= 0 -> (Left $ left_err <> right_err, tbl)
              | length left_err /= 0 -> (Left left_err, tbl)
              | length right_err /= 0 -> (Left right_err, tbl)
              | hasMacroDef subbed_left'
                || hasMacroDef subbed_right' -> (Left [E.NonGlblMacroDef s e], tbl)
              | otherwise ->
                let 
                  filtered_left = head' $ catMaybes subbed_left'
                  filtered_right = head' $ catMaybes subbed_right'
                  expr = L.SEBinary op s e filtered_left filtered_right
                in (Right $ Just expr, tbl)
macroSub (L.SEUnary op s e v) tbl = simpleSub (L.SEUnary op s e) (s, e) v tbl
macroSub (L.SEVar s e (L.Var i t v)) tbl = simpleSub (\x -> L.SEVar s e (L.Var i t x)) (s, e) v tbl
macroSub (L.SEConst s e (L.Var i t v)) tbl = simpleSub (\x -> L.SEConst s e (L.Var i t x)) (s, e) v tbl
macroSub (L.SESet s e i v) tbl = simpleSub (L.SESet s e i) (s, e) v tbl
macroSub v tbl = (Right $ Just v, tbl)

-- main typechecking/sema functions.
infer :: L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
infer (L.SEIdentifier iden s e) env = inferIdentifier s e iden env
infer (L.SELiteral ty _ _ _) env = (Right ty, env)
infer (L.SEUnary op s e v) env = checkUnary s e op v env
infer (L.SEBinary op s e l r) env = checkBinary s e op l r env
infer (L.SEVar s e v) env = inferVariable s e v env False
infer (L.SEConst s e v) env = inferVariable s e v env True
infer (L.SESet s e i v) env = inferSet s e i v env
infer (L.SEReturn _ _ v) env = infer v env
infer (L.SEPrint _ _ _) env = (Right L.Unit', env)
infer (L.SEFunc f) env = inferFunc f env
infer (L.SEBlockStmt _ _ body) env = inferBlock body env
infer (L.SEFuncCall s e iden args) env = inferFuncCall s e iden args env
infer (L.SEIfStmt s e cond tbr fbr) env = inferIfStmt s e cond tbr fbr env
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
    aux _ _ _ True _ _ = let (ty, _) = fenv M.! iden in (Right ty, env)
    aux _ _ _ _ True _ = let ty = venv M.! iden in (Right ty, env)
    aux _ _ _ _ _ True = let ty = cenv M.! iden in (Right ty, env)
    aux st end i False False False = (Left $ [E.UndefinedIdentifier st end i], env)

checkUnary :: L.LizPos -> L.LizPos -> L.UnaryOp -> L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
checkUnary s e op v env =
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

checkBinary :: L.LizPos -> L.LizPos -> L.BinaryOp -> L.SExpr -> L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
checkBinary s e op l r env =
  case (infer l env, infer r env) of
    ((Left el, env'), (Left er, _)) -> (Left $ el <> er, env')
    (err@(Left _, _), _) -> err
    (_, err@(Left _, _)) -> err
    ((Right leftType, env'), (Right rightType, _)) -> (aux op leftType rightType, env') -- can't define stuff in binary sexprs
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
    (Right ty, env') -> aux varIdent varType ty env'
  where
    aux :: T.Text -> L.Type -> L.Type -> Env -> (Either [E.SemErr] L.Type, Env)
    aux ident decType valType t@(Env {envVars=varMap, envConsts=constMap, envFuncs=funcMap})
      | ident `M.member` varMap || ident `M.member` constMap || ident `M.member` funcMap = (Left $ [E.IdentifierAlreadyInUse s e ident], env)
      | valType /= decType = 
        let newenv = M.insert ident varType -- so that you don't get a bunch of undefined variable errors.
        in if isConst then (Left $ [E.IncorrectType s e decType valType], t{envConsts=newenv constMap})
                      else (Left $ [E.IncorrectType s e decType valType], t{envVars=newenv varMap})
      | otherwise = 
        let newenv = M.insert ident decType 
        in if isConst then (Right decType, t{envConsts=newenv constMap})
                      else (Right decType, t{envVars=newenv varMap})

inferSet :: L.LizPos -> L.LizPos -> T.Text -> L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
inferSet s end ident v env =
  case (infer v env) of
    err@(Left _, _) -> err
    (Right ty, env') -> aux ident ty env'
  where
    aux :: T.Text -> L.Type -> Env -> (Either [E.SemErr] L.Type, Env)
    aux i ty e@(Env{..})
      | i `M.member` envConsts = (Left $ [E.AssigningToConstant s end i], e)
      | i `M.member` envFuncs = (Left $ [E.AssigningToFunction s end ident], e)
      | otherwise =
        let value = i `M.lookup` envVars in 
        case value of
          Nothing -> (Left $ [E.UndefinedIdentifier s end i], e)
          Just correctType | correctType == ty -> (Right ty, e)
                           | otherwise -> (Left $ [E.IncorrectType s end correctType ty], e)

inferFunc :: L.Func -> Env -> (Either [E.SemErr] L.Type, Env)
inferFunc (L.Func{..}) env@(Env{..})
  | funcIdent `M.member` envFuncs || funcIdent `M.member` envVars || funcIdent `M.member` envConsts = (Left $ [E.IdentifierAlreadyInUse funcStart funcEnd funcIdent], env)
  | otherwise =
    let
      argTypeErrs = 
        lefts $ foldMap (\L.Arg{..} -> if argType == L.Unit' then [Left (E.InvalidArgType funcStart funcEnd argIdent argType)] else [Right ()]) funcArgs
      envWithArgs = flip addArgs env $ map (\L.Arg{..} -> (argIdent, argType)) funcArgs
      result = inferBody funcBody envWithArgs
      errsAndTypes = collectErrors result [] []
    in aux errsAndTypes funcReturnType argTypeErrs
  where
    aux (errs, types) ret aterrs =
      case () of _
                  | length aterrs /= 0 && length errs /= 0 && last types /= ret -> (Left $ (E.IncorrectType funcStart funcEnd ret (last types)) : aterrs <> errs, env)
                  | length errs /= 0 && length aterrs /= 0 -> (Left $ aterrs <> errs, env)
                  | last types /= ret && length aterrs /= 0 -> (Left $ [E.IncorrectType funcStart funcEnd ret (last types)] <> aterrs, env)
                  | last types /= ret && length errs /= 0 -> (Left $ [E.IncorrectType funcStart funcEnd ret (last types)] <> errs, env)
                  | length errs /= 0 -> (Left errs, env)
                  | length aterrs /= 0 -> (Left aterrs, env)
                  | last types /= ret -> (Left $ [E.IncorrectType funcStart funcEnd ret (last types)], env)
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
    (errs, types) = collectErrors result [] []
  in
  if length errs /= 0 then (Left errs, env)
                      else (Right $ last types, env)

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

inferIfStmt :: L.LizPos -> L.LizPos -> L.SExpr -> L.SExpr -> Maybe L.SExpr -> Env -> (Either [E.SemErr] L.Type, Env)
inferIfStmt s e cond tbranch Nothing env =
  case (infer cond env, infer tbranch env) of
    ((Left ecs, _), (Left ets, env')) -> (Left $ ecs <> ets, env')
    (err@((Left _), _), _) -> err
    (_, err@(Left _, _)) -> err
    ((Right tccond, _), (Right tctbr, env')) | tccond /= L.Bool' -> (Left [E.IncorrectType s e L.Bool' tccond], env')
                                             | otherwise -> (Right tctbr, env')
inferIfStmt s e cond tbranch (Just fbranch) env =
  case (infer cond env, infer tbranch env, infer fbranch env) of
    ((Left ecs, _), (Left ets, _), (Left efs, env')) -> (Left $ ecs <> ets <> efs, env')
    (err@((Left _), _), _, _) -> err
    (_, err@(Left _, _), _) -> err
    (_, _, err@(Left _, _)) -> err
    ((Right tccond, _), (Right tctbr, _), (Right tcfbr, env')) | tccond /= L.Bool' && tctbr /= tcfbr -> (Left [E.IncorrectType s e L.Bool' tccond, E.IncorrectType s e tctbr tcfbr], env')
                                                               | tccond /= L.Bool' -> (Left [E.IncorrectType s e L.Bool' tccond], env')
                                                               | tctbr /= tcfbr -> (Left [E.IncorrectType s e tctbr tcfbr], env')
                                                               | otherwise -> (Right tcfbr, env')
