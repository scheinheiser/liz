{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Codegen (ppQBE, irToQBE) where

import qualified Liz.Common.Types as CT
import qualified Liz.QBE.QBE as Q

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Map as M

import Liz.IR.IRTypes
import Liz.IR.IR (flattenExpr)
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)

import Unsafe.Coerce (unsafeCoerce)
import Data.Word
import Data.Char (ord)

type SymbolMap = M.Map T.Text CT.Type

typeToPrim :: CT.Type -> Q.Prim
typeToPrim CT.Int' = Q.PrimWord
typeToPrim CT.Float' = Q.PrimSingle
typeToPrim CT.Bool' = Q.PrimWord
typeToPrim CT.Char' = Q.PrimWord
typeToPrim CT.String' = Q.PrimLong
typeToPrim CT.Unit' = error "Attempted conversion of unit to primitive in codegen."

fromBasicExpr :: Expr -> Q.Value
fromBasicExpr (Ident n isGlobal) = 
  if isGlobal then Q.VConst $ Q.CGlobal (Q.Ident @Q.Global n) 
              else Q.VTemp $ Q.Ident @Q.Temp n
fromBasicExpr (EVal (Str s _)) = Q.VConst $ Q.CGlobal (Q.Ident @Q.Global s)
fromBasicExpr (EVal (Integ i)) = 
  if i < 0 then Q.VConst $ Q.CInt True (unsafeCoerce $ abs i)
           else Q.VConst $ Q.CInt False (unsafeCoerce i)
fromBasicExpr (EVal (Flt f)) = Q.VConst $ Q.CFloat f
fromBasicExpr (EVal (Bln b)) = Q.VConst $ Q.CInt False (toInt b)
  where
    toInt :: Bool -> Word64
    toInt True = 1 
    toInt False = 0
fromBasicExpr (EVal (Chr c)) = Q.VConst $ Q.CInt False (unsafeCoerce $ ord c)
fromBasicExpr _ = error "Internal error - nested expressions must be flattened." -- should be unreachable

ppQBE :: Q.Program -> IO ()
ppQBE = putDoc . pretty

irToQBE :: IR -> Q.Program
irToQBE (IR funcs glbls strs _ _ _ symmap) =
  let
    strs' = 
      map 
        (\(lit, n) -> 
          let 
            lit' = 
              if "fbuf" `T.isPrefixOf` n
              then 
                let (_, am) = T.breakOn ":" lit in
                Q.DIZero (read @Int . T.unpack . T.drop 1 $ am)
              else Q.DIString lit 
          in Q.DataDef (Q.Ident @Q.Global n) Nothing (NE.singleton lit')) strs
    glbls' = map (\(Variable i _ expr) -> Q.DataDef (Q.Ident @Q.Global i) Nothing (NE.singleton $ fromGlblValue expr)) glbls
    funcs' = map (flip funcToQBE symmap) funcs
  in Q.Program funcs' (strs' <> glbls') []
  where
    fromGlblValue :: Expr -> Q.DataItem
    fromGlblValue (EVal v) = Q.DIConst $ valueToConst v
    fromGlblValue (Ident i _) = Q.DIConst $ Q.CGlobal (Q.Ident @Q.Global i) 
    fromGlblValue _ = error "Internal error - cannot have expressions within a global variable." -- should be unreachable

    valueToConst :: Val -> Q.Const
    valueToConst (Integ i) =
      if i < 0 then Q.CInt True (unsafeCoerce $ abs i)
               else Q.CInt False (unsafeCoerce i)
    valueToConst (Flt f) = Q.CFloat f
    valueToConst (Bln b) = Q.CInt False (toInt b)
      where
        toInt :: Bool -> Word64
        toInt True = 1 
        toInt False = 0
    valueToConst (Chr c) = Q.CInt False (unsafeCoerce $ ord c)
    valueToConst (Str s _) = Q.CGlobal (Q.Ident @Q.Global s)

    funcToQBE :: Fn -> SymbolMap -> Q.FuncDef
    funcToQBE (Fn ident args body ret) symmap' =
      let
        ident' = Q.Ident @Q.Global ident
        ret' =
          case ret of
            CT.Unit' -> Nothing
            _ -> Just $ Q.AbiPrim (typeToPrim ret)
        args' = argsToParams args 
        body' = foldMap (flip fromCFlow symmap') body
        linkage =
          if ident == "main" then (Just Q.Export) else Nothing
      in Q.FuncDef linkage ret' ident' args' (NE.fromList body')
      where
        argsToParams :: [CT.Arg] -> [Q.Param]
        argsToParams [] = []
        argsToParams (CT.Arg{..} : as) =
          let
            argIdent' = Q.VTemp $ Q.Ident @Q.Temp argIdent
            -- a unit arg would cause a semantic error, and so isn't a concern here.
            argType' = Q.AbiPrim $ typeToPrim argType
          in (Q.RegularParam argIdent' argType') : argsToParams as

fromCFlow :: CFlow -> SymbolMap -> [Q.Block]
fromCFlow (Lbl (Label (i, body))) symmap =
  let
    i' = Q.Ident @Q.Label i
    body' = foldMap (flip fromIROp symmap) body
  in [Q.Block i' body']
fromCFlow (BlockStmt i body) symmap =
  let
    -- the empty block will lead to an automatic jump to the next label.
    -- this preserves the goto from the previous cflow to this block
    i' = Q.Block (Q.Ident @Q.Label i) []
    body' = foldMap (flip fromCFlow symmap) body
  in [i'] <> body'
fromCFlow (IfStmt i cond main (Label (tlbl, tbody)) Nothing) symmap =
  let
    i' = Q.Ident @Q.Label i
    main' = Q.Ident @Q.Label main
    tlbl' = Q.Ident @Q.Label tlbl
    flattened_cond = flattenExpr cond
    cond' = foldMap (flip fromExpr symmap) flattened_cond 
    tbody' = Q.Block tlbl' (foldMap (flip fromIROp symmap) tbody)
    cond_block = Q.Block i' (cond' <> [Q.Jump $ Q.Jnz (getCond $ last flattened_cond) tlbl' main'])
  in [cond_block, tbody']
  where
    getCond :: Expr -> Q.Value
    getCond v@((EVal _);(Ident _ _)) = fromBasicExpr v
    getCond (Bin vi _ _ _ _) = Q.VTemp $ Q.Ident @Q.Temp vi
    getCond (Un vi _ _ _) = Q.VTemp $ Q.Ident @Q.Temp vi
    getCond (FuncCall vi _ _ _) = Q.VTemp $ Q.Ident @Q.Temp vi
    getCond _ = error "Internal error - there should never be return/print/phi in condition."
fromCFlow (IfStmt i cond _ (Label (tlbl, tbody)) (Just (Label (flbl, fbody)))) symmap =
  let
    i'    = Q.Ident @Q.Label i
    tlbl' = Q.Ident @Q.Label tlbl
    flbl' = Q.Ident @Q.Label flbl
    flattened_cond = flattenExpr cond
    cond' = foldMap (flip fromExpr symmap) flattened_cond 
    tbody' = Q.Block tlbl' (foldMap (flip fromIROp symmap) tbody)
    fbody' = Q.Block flbl' (foldMap (flip fromIROp symmap) fbody)
    cond_block = Q.Block i' (cond' <> [Q.Jump $ Q.Jnz (getCond $ last flattened_cond) tlbl' flbl'])
  in [cond_block, tbody', fbody']
  where
    getCond :: Expr -> Q.Value
    getCond v@((EVal _);(Ident _ _)) = fromBasicExpr v
    getCond (Bin vi _ _ _ _) = Q.VTemp $ Q.Ident @Q.Temp vi
    getCond (Un vi _ _ _) = Q.VTemp $ Q.Ident @Q.Temp vi
    getCond (FuncCall vi _ _ _) = Q.VTemp $ Q.Ident @Q.Temp vi
    getCond _ = error "Internal error - there should never be return/print/phi in condition."

fromIROp :: IROp -> SymbolMap -> [Q.Instr]
fromIROp (IRGoto n) _ = [Q.Jump $ Q.Jmp (Q.Ident @Q.Label n)]
fromIROp (IRExpr e) symmap = fromExpr e symmap
fromIROp (IRVar (Variable i _ (Bin _ t op l r))) symmap = flip fromExpr symmap $ Bin i t op l r
fromIROp (IRVar (Variable i _ (Un _ t op v))) symmap = flip fromExpr symmap $ Un i t op v
fromIROp (IRVar (Variable i _ (FuncCall _ t func vs))) symmap = flip fromExpr symmap $ FuncCall i t func vs
fromIROp (IRVar (Variable i t v@((EVal _);(Ident _ _)))) _ =
  let
    t' = typeToPrim t
    i' = Q.Assignment (Q.Ident @Q.Temp i) (Q.AbiPrim t')
    v' = fromBasicExpr v
  in [Q.Unary i' Q.Copy v']
fromIROp (IRVar _) _ = error "Internal error - there shouldn't be return/print/phi in variable."

fromExpr :: Expr -> SymbolMap -> [Q.Instr]
fromExpr (Ret v) _ = [Q.Jump $ Q.Ret (Just $ fromBasicExpr v)]
fromExpr (Print v) _ = [Q.Call Nothing (Q.Ident @Q.Global "printf") [Q.RegularParam (fromBasicExpr v) (Q.AbiPrim Q.PrimLong)]]
fromExpr (Bin ident ty operator left right) _ = formatBinary ident ty operator left right
  where 
    formatBinary :: T.Text -> CT.Type -> CT.BinaryOp -> Expr -> Expr -> [Q.Instr]
    formatBinary i t op l r =
      let
        t' = typeToPrim t
        i' = Q.Assignment (Q.Ident @Q.Temp i) (Q.AbiPrim t')
        op' = binaryToQBE op t'
        l' = fromBasicExpr l
        r' = fromBasicExpr r
      in [Q.Binary i' op' l' r']

    binaryToQBE :: CT.BinaryOp -> Q.Prim -> Q.BinOp
    binaryToQBE CT.Add _ = Q.Add
    binaryToQBE CT.Subtract _ = Q.Sub
    binaryToQBE CT.Multiply _ = Q.Mul
    binaryToQBE CT.Divide _ = Q.Div
    binaryToQBE CT.Greater t = 
      case t of
        Q.PrimWord;Q.PrimLong -> Q.CSge t
        Q.PrimSingle;Q.PrimDouble -> Q.CGt t
    binaryToQBE CT.Less t =
      case t of
        Q.PrimWord;Q.PrimLong -> Q.CSlt t
        Q.PrimSingle;Q.PrimDouble -> Q.CLt t
    binaryToQBE CT.GreaterEql t =
      case t of
        Q.PrimWord;Q.PrimLong -> Q.CSge t
        Q.PrimSingle;Q.PrimDouble -> Q.CGe t
    binaryToQBE CT.LessEql t =
      case t of
        Q.PrimWord;Q.PrimLong -> Q.CSle t
        Q.PrimSingle;Q.PrimDouble -> Q.CLe t
    binaryToQBE CT.Eql t = Q.CEq t
    binaryToQBE CT.NotEql t = Q.CNe t
    binaryToQBE CT.Concat _ = error "figure out concat operator."

fromExpr (Un ident ty operator value) _ = formatUnary ident ty operator value
  where 
    formatUnary :: T.Text -> CT.Type -> CT.UnaryOp -> Expr -> [Q.Instr]
    formatUnary i t CT.Negate v =
      let
        i' = Q.Assignment (Q.Ident @Q.Temp i) (Q.AbiPrim $ typeToPrim t)
        v' = fromBasicExpr v
      in [Q.Unary i' Q.Neg v']
    formatUnary i t CT.Not v =
      let
        t' = typeToPrim t
        i' = Q.Assignment (Q.Ident @Q.Temp i) (Q.AbiPrim $ typeToPrim t)
        v' = fromBasicExpr v
         -- check equality to 0 (false)
      in [Q.Binary i' (Q.CEq t') v' (Q.VConst $ Q.CInt False 0)]
fromExpr (Format bufident (str, _) exprs) symmap =
  let
    bufident' = Q.RegularParam (Q.VConst . Q.CGlobal $ Q.Ident @Q.Global bufident) (Q.AbiPrim Q.PrimLong)
    str' = [bufident', Q.RegularParam (Q.VConst . Q.CGlobal $ Q.Ident @Q.Global str) (Q.AbiPrim Q.PrimLong), Q.VariadicParam]
    exprs' = map exprToParam exprs
  in [Q.Call Nothing (Q.Ident @Q.Global "sprintf") $ str' <> exprs']
  where
    exprToParam :: Expr -> Q.Param
    exprToParam v@(EVal (Str _ _)) = Q.RegularParam (fromBasicExpr v) (Q.AbiPrim Q.PrimLong)
    exprToParam v@((EVal (Chr _));(EVal (Integ _))) = Q.RegularParam (fromBasicExpr v) (Q.AbiPrim Q.PrimWord)
    exprToParam v@(EVal (Flt _)) = Q.RegularParam (fromBasicExpr v) (Q.AbiPrim Q.PrimSingle)
    exprToParam i@(Ident ident'' _) = 
      let t = symmap M.! ident'' in
      Q.RegularParam (fromBasicExpr i) (Q.AbiPrim $ typeToPrim t)
fromExpr (Phi i t branches) _ =
  let
    i' = Q.Assignment (Q.Ident @Q.Temp i) (Q.AbiPrim $ typeToPrim t)
    branches' = map (\(lbl, value) -> ((Q.Ident @Q.Label lbl), fromBasicExpr value)) branches
  in [Q.Phi i' (NE.fromList branches')]
fromExpr e _ = error $ "Internal error - unhandled expression: " <> (show e)
