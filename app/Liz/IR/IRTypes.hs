{-# LANGUAGE OverloadedStrings #-}

module Liz.IR.IRTypes (AllocTracker (..), IROp (..), CFlow (..), Expr (..), Val (..), Label (..), Goto) where

import qualified Liz.Common.Types as L
import qualified Data.Text as T
import qualified Data.Map as M

import Prettyprinter

data AllocTracker = AllocTracker
  { atAllocatedStrings :: [T.Text]
  , atStringIdx :: Int
  , atSymbols :: M.Map T.Text L.Type
  } deriving Show

type Goto = T.Text
newtype Label = Label (T.Text, [IROp]) -- name, expressions
  deriving Show

instance Pretty Label where
  pretty (Label (name, exprs)) =
    (pretty @T.Text $ name <> ":") 
      <> line <> (indent 2 . vcat $ map pretty exprs) 
        <> line

data Val = Integ Int
  | Bln Bool
  | Flt Double
  | Str T.Text
  | Chr Char
  | Unt 
  deriving Show

instance Pretty Val where
  pretty (Integ i) = viaShow i
  pretty (Flt i) = viaShow i
  pretty (Bln i) = viaShow i
  pretty (Chr i) = viaShow i
  pretty (Str i) = (pretty @T.Text "string") <> lbracket <> (pretty i) <> rbracket
  pretty Unt = pretty @T.Text "()"

data Expr = Bin T.Text L.BinaryOp Expr Expr
  | Un T.Text L.UnaryOp Expr
  | Ret Expr
  | Print Expr
  | Phi T.Text [(T.Text, Expr)] -- identifier - label names + the value associated with it
  | FuncCall T.Text [Expr]
  | Ident T.Text
  | EVal Val
  deriving Show

instance Pretty Expr where
  pretty (Ident i) = pretty @T.Text i
  pretty (Bin i op l r) = formatBinary i op l r
  pretty (Un i op v) = formatUnary i op v
  pretty (Ret v) = formatPrintOrRet "ret" v
  pretty (Print v) = formatPrintOrRet "print" v
  pretty (Phi i branches) = 
    (pretty i) <+> (pretty @T.Text "=") 
        <+> (pretty @T.Text "phi") 
          <+> (hsep . punctuate comma $ map (\(b, r) -> (pretty b) <+> (pretty r)) branches)
  pretty (FuncCall i exprs) = 
    (pretty @T.Text i) 
      <> (parens . hsep . punctuate comma $ map pretty exprs)
  pretty (EVal v) = pretty v

type IRBlock = [IROp]

data CFlow = IfStmt Expr Goto Label (Maybe Label) -- cond - true branch - optional false branch
  | Lbl Label
  | BlockStmt IRBlock
  | Fn T.Text [L.Arg] [IROp] L.Type -- identifier - exprs - return type
  | CGoto Goto -- a wrapper around goto for control flow/leader algorithm
  deriving Show

instance Pretty CFlow where
  pretty (IfStmt cond gotomain truebranch falsebranch) = formatIfStmt cond gotomain truebranch falsebranch
  pretty (Lbl lbl) = pretty lbl
  pretty (BlockStmt exprs) =
    (pretty @T.Text "block:") <> line <> (indent 2 . vcat $ map pretty exprs)
  pretty (CGoto name) = (pretty @T.Text "goto") <+> (pretty name)
  pretty (Fn i args body retTy) =
    (pretty @T.Text i) <> (encloseSep lparen rparen (comma <> space) $ map prettifyArg args) 
      <+> (pretty @T.Text "->") <+> (viaShow retTy) 
        <> (pretty @T.Text ":") <> line 
          <> (indent 2 . vcat $ map pretty body)

data IROp = IRExpr Expr
  | IRFlow CFlow
  | Var T.Text L.Type Expr
  deriving Show

instance Pretty IROp where
  pretty (IRExpr ex) = pretty ex
  pretty (IRFlow flow) = pretty flow
  pretty (Var i t v) = formatVar i t v

-- helper pretty functions
formatVar :: T.Text -> L.Type -> Expr -> Doc ann
formatVar ident t v@(Bin i _ _ _) = 
  (pretty v) <> line 
    <> (pretty t) <+> (pretty ident) 
      <+> (pretty @T.Text "=") <+> (pretty i)
formatVar ident t v@(Un i _ _) = 
  (pretty v) <> line 
    <> (pretty t) <+> (pretty ident) 
      <+> (pretty @T.Text "=") <+> (pretty i)
formatVar ident t v = 
  (pretty t) <+> (pretty ident) 
    <+> (pretty @T.Text "=") <+> (pretty v)

formatBinary :: T.Text -> L.BinaryOp -> Expr -> Expr -> Doc ann
formatBinary i op l@(Bin li _ _ _) r@(Bin ri _ _ _) =
  (pretty l) <> line 
    <> (pretty r) <> line 
      <> (pretty i) <+> (pretty @T.Text "=") <+> (pretty li) 
          <+> (pretty $ binaryToText op) <+> (pretty ri)
formatBinary i op l@(Un li _ _) r@(Bin ri _ _ _) =
  (pretty l) <> line 
    <> (pretty r) <> line 
      <> (pretty i) <+> (pretty @T.Text "=") 
        <+> (pretty li) <+> (pretty $ binaryToText op) 
          <+> (pretty ri)
formatBinary i op l@(Bin li _ _ _) r@(Un ri _ _) =
  (pretty l) <> line 
    <> (pretty r) <> line 
      <> (pretty i) <+> (pretty @T.Text "=") 
        <+> (pretty li) <+> (pretty $ binaryToText op) 
          <+> (pretty ri)
formatBinary i op l@(Un li _ _) r@(Un ri _ _) =
  (pretty l) <> line 
    <> (pretty r) <> line 
      <> (pretty i) <+> (pretty @T.Text "=") 
        <+> (pretty li) <+> (pretty $ binaryToText op) 
          <+> (pretty ri)
formatBinary i op l r =
  (pretty i) <+> (pretty @T.Text "=") 
    <+> (pretty l) <+> (pretty $ binaryToText op) 
      <+> (pretty r)

formatUnary :: T.Text -> L.UnaryOp -> Expr -> Doc ann
formatUnary i op v@(Un vi _ _) =
  (pretty v) <> line 
    <> (pretty i) <+> (pretty @T.Text "=") 
      <+> (pretty $ unaryToText op) <+> (pretty vi) 
formatUnary i op v@(Bin vi _ _ _) =
  (pretty v) <> line 
    <> (pretty i) <+> (pretty @T.Text "=") 
      <+> (pretty $ unaryToText op) <+> (pretty vi) 
formatUnary i op v =
  (pretty i) <+> (pretty @T.Text "=") 
    <+> (pretty $ unaryToText op) <+> (pretty v)

formatPrintOrRet :: T.Text -> Expr -> Doc ann
formatPrintOrRet dec v@(Bin i _ _ _) = 
  (pretty v) <> line 
    <> (pretty dec) 
      <+> (pretty i)
formatPrintOrRet dec v@(Un i _ _) = 
  (pretty v) <> line 
    <> (pretty dec) 
      <+> (pretty i)
formatPrintOrRet dec v = (pretty @T.Text dec) <+> (pretty v)

prettifyArg :: L.Arg -> Doc ann
prettifyArg (L.Arg {argIdent=name, argType=ty}) = (pretty @T.Text $ name <> ":") <+> (viaShow ty)

formatIfStmt :: Expr -> Goto -> Label -> Maybe Label -> Doc ann
formatIfStmt cond@(Bin ident _ _ _) gotomain lbl@(Label (gototrue, _)) Nothing =
  (pretty cond) <> line 
    <> (pretty @T.Text "if") 
      <+> (pretty ident) <+> (pretty @T.Text "then goto") 
        <+> (pretty gototrue) <+> (pretty @T.Text "else goto") 
          <+> (pretty gotomain) <> line 
            <> (pretty $ Lbl lbl)
formatIfStmt cond@(Un ident _ _) gotomain lbl@(Label (gototrue, _)) Nothing =
  (pretty cond) <> line 
    <> (pretty @T.Text "if") <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotomain) 
          <> line <> (pretty $ Lbl lbl)
formatIfStmt cond gotomain lbl@(Label (gototrue, _)) Nothing =
  (pretty @T.Text "if") <+> (pretty cond) 
    <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
      <+> (pretty @T.Text "else goto") <+> (pretty gotomain) 
        <> line <> (pretty $ Lbl lbl)
formatIfStmt cond@(Bin ident _ _ _) _ tlbl@(Label (gototrue, _)) (Just (flbl@(Label (gotofalse, _)))) =
  (pretty cond) <> line 
    <> (pretty @T.Text "if") <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
          <> line <> (pretty $ Lbl tlbl) 
            <> (pretty $ Lbl flbl)
formatIfStmt cond@(Un ident _ _) _ tlbl@(Label (gototrue, _)) (Just (flbl@(Label (gotofalse, _)))) =
  (pretty cond) <> line 
    <> (pretty @T.Text "if") <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
          <> line <> (pretty $ Lbl tlbl) 
            <> (pretty $ Lbl flbl)
formatIfStmt cond _ tlbl@(Label (gototrue, _)) (Just (flbl@(Label (gotofalse, _)))) =
  (pretty @T.Text "if") <+> (pretty cond) 
    <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
      <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
        <> line <> (pretty $ Lbl tlbl) 
          <> (pretty $ Lbl flbl)

binaryToText :: L.BinaryOp -> T.Text
binaryToText L.Add = "+"
binaryToText L.Subtract = "-"
binaryToText L.Multiply = "*"
binaryToText L.Divide = "/"
binaryToText L.Greater = ">"
binaryToText L.GreaterEql = ">="
binaryToText L.Less = "<"
binaryToText L.LessEql = "<="
binaryToText L.Eql = "=="
binaryToText L.NotEql = "!="
binaryToText L.Concat = "++"

unaryToText :: L.UnaryOp -> T.Text
unaryToText L.Not = "not"
unaryToText L.Negate = "negate"
