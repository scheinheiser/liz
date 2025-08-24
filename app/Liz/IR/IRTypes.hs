{-# LANGUAGE OverloadedStrings #-}

module Liz.IR.IRTypes (IR (..), Fn (..), Variable (..), IROp (..), CFlow (..), Expr (..), Val (..), Label (..), Goto, SymbolMap) where

import qualified Liz.Common.Types as CT
import qualified Data.Text as T
import qualified Data.Map as M

import Prettyprinter

type Goto = T.Text
type LabelIdent = T.Text
type InternalStringIdent = T.Text
type SymbolMap = M.Map T.Text CT.Type
newtype Label = Label (LabelIdent, [IROp]) -- name, expressions
  deriving Show

instance Pretty Label where
  pretty (Label (name, exprs)) =
    (pretty @T.Text $ name <> ":") 
      <> line <> (indent 2 . vcat $ map pretty exprs) 
        <> line

data Val = Integ Int
  | Bln Bool
  | Flt Double
  | Str InternalStringIdent T.Text
  | Chr Char
  | Unt 
  deriving Show

instance Pretty Val where
  pretty (Integ i) = viaShow i
  pretty (Flt i) = viaShow i
  pretty (Bln i) = viaShow i
  pretty (Chr i) = viaShow i
  pretty (Str i _) = (pretty @T.Text "string") <> lbracket <> (pretty $ T.drop 3 i) <> rbracket
  pretty Unt = pretty @T.Text "()"

data Expr = Bin T.Text CT.Type CT.BinaryOp Expr Expr
  | Un T.Text CT.Type CT.UnaryOp Expr
  | Ret Expr
  | Print Expr
  | Phi T.Text CT.Type [(LabelIdent, Expr)] -- identifier - label names + the value associated with it
  | FuncCall T.Text CT.Type T.Text [Expr]
  | Ident T.Text Bool -- name, flag if it's global.
  | EVal Val
  | Format T.Text (InternalStringIdent, T.Text) [Expr] -- interal buffer ident, (internal string ident, format string), format params
  | BreakStmt T.Text
  deriving Show

instance Pretty Expr where
  pretty (Ident i _) = pretty @T.Text i
  pretty (Bin i t op l r) =
    (pretty t) <+> (pretty i) 
      <+> (pretty @T.Text "=") <+> (pretty l) 
        <+> (pretty $ binaryToText op) <+> (pretty r)
  pretty (Un i t op v) =
    (pretty t) <+> (pretty i) 
      <+> (pretty @T.Text "=") <+> (pretty $ unaryToText op) 
        <+> (pretty v)
  pretty (Ret v) = (pretty @T.Text "ret") <+> (pretty v)
  pretty (Print v) = (pretty @T.Text "print") <+> (pretty v)
  pretty (Phi i t branches) = 
    (pretty t) <+> (pretty i) 
      <+> (pretty @T.Text "=") <+> (pretty @T.Text "phi") 
          <+> (hsep . punctuate comma $ map (\(b, r) -> (pretty b) <+> (pretty r)) branches)
  pretty (FuncCall _ _ i exprs) = 
    (pretty @T.Text i) 
      <> (parens . hsep . punctuate comma $ map pretty exprs)
  pretty (EVal v) = pretty v
  pretty (Format n (_, fstr) []) =
    (pretty n) <+> (pretty @T.Text "=") 
      <+> (pretty @T.Text "format") <> (parens $ pretty fstr)
  pretty (Format n (_, fstr) exprs) =
    let body = pretty fstr : (map pretty exprs) in
    (pretty n) <+> (pretty @T.Text "=") 
      <+> (pretty @T.Text "format") <> (parens . hsep $ punctuate comma body)
  pretty (BreakStmt n) =
    (pretty @T.Text "break") <+> (pretty n)

type IRBlock = [CFlow]

data CFlow = IfStmt LabelIdent Expr Goto Label (Maybe Label) -- if statement id (for codegen) - cond - true branch - optional false branch
  | Lbl Label
  | BlockStmt LabelIdent IRBlock
  | UntilStmt LabelIdent Expr Goto (Goto, [CFlow])
  deriving Show

instance Pretty CFlow where
  pretty (IfStmt n cond gotomain truebranch falsebranch) = formatIfStmt n cond gotomain truebranch falsebranch
  pretty (Lbl lbl) = pretty lbl
  pretty (BlockStmt n exprs) =
    (pretty @T.Text $ n <> ":") <> line <> (indent 2 . vcat $ map pretty exprs)
  pretty (UntilStmt n cond gotomain body) = formatUntilStmt n cond gotomain body

data Variable = Variable T.Text CT.Type Expr
  deriving Show

instance Pretty Variable where
  pretty (Variable i t v) =
    (pretty t) <+> (pretty i) 
      <+> (pretty @T.Text "=") <+> (pretty v)

data IROp = IRExpr Expr
  | IRVar Variable
  | IRGoto Goto 
  deriving Show

instance Pretty IROp where
  pretty (IRExpr ex) = pretty ex
  pretty (IRVar v) = pretty v 
  pretty (IRGoto name) = (pretty @T.Text "goto") <+> (pretty name)

data Fn = Fn T.Text [CT.Arg] [CFlow] CT.Type -- identifier - exprs - return type
  deriving Show
instance Pretty Fn where
  pretty (Fn i args body retTy) =
    (pretty @T.Text i) <> (encloseSep lparen rparen (comma <> space) $ map prettifyArg args) 
      <+> (pretty @T.Text "->") <+> (viaShow retTy) 
        <> (pretty @T.Text ":") <> line 
          <> (indent 2 . vcat $ map pretty body)
    where
      prettifyArg :: CT.Arg -> Doc ann
      prettifyArg (CT.Arg {argIdent=name, argType=ty}) = (pretty @T.Text $ name <> ":") <+> (viaShow ty)

data IR = IR 
  { irFuncs            :: [Fn]
  , irGlbls            :: [Variable]
  , irAllocatedStrings :: [(T.Text, T.Text)] -- lit, 'index'
  , irStringIdx        :: !Int
  , irTempVarIdx       :: !Int -- to provide identifiers for temporary variables
  , irCFlowIdx         :: !Int -- to provide labels for control flow statements.
  , irSymbols          :: SymbolMap
  } deriving Show

-- helper pretty functions
formatIfStmt :: T.Text -> Expr -> Goto -> Label -> Maybe Label -> Doc ann
formatIfStmt id' cond@(Bin ident _ _ _ _) gotomain lbl@(Label (gototrue, _)) Nothing =
  (pretty cond) <> line 
    <> (pretty @T.Text id') 
      <+> (pretty ident) <+> (pretty @T.Text "then goto") 
        <+> (pretty gototrue) <+> (pretty @T.Text "else goto") 
          <+> (pretty gotomain) <> line 
            <> (pretty lbl)
formatIfStmt id' cond@(Un ident _ _ _) gotomain lbl@(Label (gototrue, _)) Nothing =
  (pretty cond) <> line 
    <> (pretty @T.Text id') <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotomain) 
          <> line <> (pretty lbl)
formatIfStmt id' cond gotomain lbl@(Label (gototrue, _)) Nothing =
  (pretty @T.Text id') <+> (pretty cond) 
    <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
      <+> (pretty @T.Text "else goto") <+> (pretty gotomain) 
        <> line <> (pretty lbl)
formatIfStmt id' cond@(Bin ident _ _ _ _) _ tlbl@(Label (gototrue, _)) (Just (flbl@(Label (gotofalse, _)))) =
  (pretty cond) <> line 
    <> (pretty @T.Text id') <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
          <> line <> (pretty $ Lbl tlbl) 
            <> (pretty flbl)
formatIfStmt id' cond@(Un ident _ _ _) _ tlbl@(Label (gototrue, _)) (Just (flbl@(Label (gotofalse, _)))) =
  (pretty cond) <> line 
    <> (pretty @T.Text id') <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
          <> line <> (pretty $ Lbl tlbl) 
            <> (pretty flbl)
formatIfStmt id' cond _ tlbl@(Label (gototrue, _)) (Just (flbl@(Label (gotofalse, _)))) =
  (pretty @T.Text id') <+> (pretty cond) 
    <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
      <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
        <> line <> (pretty $ Lbl tlbl) 
          <> (pretty flbl)

formatUntilStmt :: T.Text -> Expr -> Goto -> (Goto, [CFlow]) -> Doc ann
formatUntilStmt id' cond@(Bin ident _ _ _ _) gotomain (gotofirst, exprs) = 
  (pretty cond) <> line 
    <> (pretty id') <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gotofirst) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotomain)
          <> line <> (indent 2 . vcat $ map pretty exprs) 
formatUntilStmt id' cond@(Un ident _ _ _) gotomain (gotofirst, exprs) = 
  (pretty cond) <> line 
    <> (pretty id') <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gotofirst) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotomain)
          <> line <> (indent 2 . vcat $ map pretty exprs) 
formatUntilStmt id' cond gotomain (gotofirst, exprs) = 
  (pretty id') <+> (pretty cond) 
    <+> (pretty @T.Text "goto") <+> (pretty gotofirst) 
      <+> (pretty @T.Text "otherwise goto") <+> (pretty gotomain)
        <> line <> (indent 2 . vcat $ map pretty exprs) 

binaryToText :: CT.BinaryOp -> T.Text
binaryToText CT.Add = "+"
binaryToText CT.Subtract = "-"
binaryToText CT.Multiply = "*"
binaryToText CT.Divide = "/"
binaryToText CT.Greater = ">"
binaryToText CT.GreaterEql = ">="
binaryToText CT.Less = "<"
binaryToText CT.LessEql = "<="
binaryToText CT.Eql = "=="
binaryToText CT.NotEql = "!="
binaryToText CT.Concat = "++"

unaryToText :: CT.UnaryOp -> T.Text
unaryToText CT.Not = "not"
unaryToText CT.Negate = "negate"
