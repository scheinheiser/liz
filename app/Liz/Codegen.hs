{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OrPatterns #-}

module Liz.Codegen where

import qualified Liz.Common.Types as CT
import qualified Liz.QBE.QBE as Q

import Liz.IR.IRTypes

{- 
IDEA:
from each translated function/label, make a program
use (<>) to mash each thing together
build it up from the ground, return the full prog 

NOTES:
I know that any given IR of a program in list form will consist of functions at the top level
Inside these functions will consist of only labels (for basic statements) or control flow
Control flow subsumes labels

I can, therefore, just map over the prog list assuming it contains just functions (otherwise error out with malformed IR).
I can map over the functions, assuming it contains only labels/control flow.
Labels/control flow will only contain basic instructions, etc etc.

From this, I'll take this approach:
  For each function, I make a new instance of the Program data type from the QBE module that contains the function.
  Smash them all together with (<>), building up the program's func decls.

  Get all data decls (strings and stuff) and translate them.
  Get all type decls (obv the impl will come later) and translate them

  Push them to the Program with all the functions, and 'writeProgToFile' that.
-}

-- we work with abitys as they contain most of the types that will be needed.
-- at worst, I can just repackage it as a subty/prim/etc when necessary
primToType :: CT.Type -> AbiTy
primToType CT.Int' = AbiPrim PrimWord
primToType CT.Float' = AbiPrim PrimSingle
-- both chars and bools are words for now, unless I find a better representation
primToType CT.Bool' = AbiPrim PrimWord
primToType CT.Char' = AbiPrim PrimWord
primToType CT.String' = AbiPrim PrimLong
primToType CT.Unit' = error "Attempted conversion of unit to abity in codegen."

opToQBE :: CT.BinaryOp -> Prim -> Q.BinOp
opToQBE CT.Add _ = Q.Add
opToQBE CT.Subtract _ = Q.Sub
opToQBE CT.Multiply _ = Q.Mul
opToQBE CT.Divide _ = Q.Div
opToQBE CT.Greater t = Q.CGt t
opToQBE CT.Less t = Q.CLt t
opToQBE CT.GreaterEql = Q.CGe t
opToQBE CT.LessEql = Q.CLe t
opToQBE CT.Eql = Q.CEq t
opToQBE CT.NotEql = Q.CNe t
opToQBE CT.Concat _ = error "figure out concat operator."

fromIRFunc :: Maybe Q.Linkage -> Fn -> FuncDef
fromIRFunc linkage (Fn i args body return) =
  let
    i' = Q.Ident @Q.Global i
    args' = map argToQBE args
    return' =
      case return of
        CT.Unit' -> Nothing
        t -> Just $ primToType t
    body' = fromIRBody body
  in FuncDef linkage return' i' args' (NE.fromList body')
  where
    argToQBE :: CT.Arg -> Q.Param
    argToQBE CT.Arg{..} = Q.RegularParam (Q.Ident @Q.Temp argIdent) (primToType argType)

fromIRBody :: [IROp] -> [Q.Block]
fromIRBody (IRLabel (Label (n, exprs)) : rest) = 
  let
    n' = Q.Ident @Q.Label n
    exprs' = map fromIRExpr exprs
  in (Q.Block n' exprs') : fromIRBody rest

fromIRExpr :: IROp -> [Q.Instr]
fromIRExpr (IRBin n op l r) = 
  let
    n' = Q.Ident @Q.Temp n
    op' = let (AbiPrim t) = primToType l in opToQBE op t
    l' = primToValue l
    r' = primToValue r
  in Q.Binary n' op' l' r'
