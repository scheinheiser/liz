{-# LANGUAGE OverloadedStrings #-}

module Liz.IR.IRTypes (IR (..), IROp (..), Label (..), Goto) where

import qualified Liz.Common.Types as L
import Prettyprinter
import qualified Data.Text as T

-- TODO: change IR data structures to have nested types.

data IR = IR 
  { irAllocatedStrings :: [T.Text]
  , irStringIdx :: Int
  } deriving Show

data IROp = IRInt Int
  | IRBool Bool 
  | IRFloat Double
  | IRString T.Text
  | IRChar Char 
  | IRUnit
  | IRUndef
  | IRIdent T.Text
  | IRBin L.BinaryOp IROp IROp
  | IRUn L.UnaryOp IROp
  | IRRet IROp
  | IRPrint IROp
  | IRVar T.Text IROp
  | IRConst T.Text IROp
  | IRFunc T.Text [L.Arg] [IROp] L.Type -- identifier - exprs - return type
  | IRFuncCall T.Text [IROp]
  | IRBlockStmt [IROp]
  | IRPhi T.Text [(T.Text, IROp)] -- identifier - possible label names + their result
  | IRIf IROp Goto (Goto, Label) (Maybe (Goto, Label)) -- cond - true branch - optional false branch
  | IRLabel Label -- a wrapper around the label for the leader algorithm
  | IRGoto Goto -- a wrapper around goto for control flow/leader algorithm
  deriving Show

type Goto = T.Text
newtype Label = Label (T.Text, [IROp]) -- name, expressions
  deriving Show

instance Pretty IROp where
  pretty (IRInt v) = viaShow v
  pretty (IRFloat v) = viaShow v
  pretty (IRBool v) = viaShow v
  pretty (IRString v) = (pretty @T.Text "string[") <> (pretty v) <> (pretty @T.Text "]")
  pretty (IRChar v) = viaShow v
  pretty (IRUndef) = pretty @T.Text "undefined"
  pretty (IRUnit) = pretty @T.Text "()"
  pretty (IRIdent i) = pretty @T.Text i
  pretty (IRBin op l r) = formatBinary op l r
  pretty (IRUn op v) = formatUnary op v
  pretty (IRRet v) = formatPrintOrRet "ret" v
  pretty (IRPrint v) = formatPrintOrRet "print" v
  pretty (IRVar i v) = formatVar "var" i v
  pretty (IRPhi i branches) = 
    (pretty @T.Text "var") <+> (pretty i)
      <+> (pretty @T.Text "=") 
        <+> (pretty @T.Text "phi") 
          <+> (hsep . punctuate comma $ map (\(b, r) -> (pretty b) <+> (pretty r)) branches)
  pretty (IRConst i v) = formatVar "const" i v
  pretty (IRFunc i args body retTy) =
    (pretty @T.Text i) <> (encloseSep lparen rparen (comma <> space) $ map prettifyArg args) 
      <+> (pretty @T.Text "->") <+> (viaShow retTy) 
        <> (pretty @T.Text ":") <> line 
          <> (indent 2 . vcat $ map pretty body)
  pretty (IRIf cond gotomain truebranch falsebranch) = formatIfStmt cond gotomain truebranch falsebranch
  pretty (IRLabel (Label (name, exprs))) =
    (pretty @T.Text $ name <> ":") 
      <> line <> (indent 2 . vcat $ map pretty exprs) 
        <> line
  pretty (IRGoto name) = (pretty @T.Text "goto") <+> (pretty name)
  pretty (IRFuncCall i exprs) = 
    (pretty @T.Text i) 
      <> (parens . hsep . punctuate comma $ map pretty exprs)
  pretty (IRBlockStmt exprs) = 
    (pretty @T.Text "block:") <> line <> (indent 2 . vcat $ map pretty exprs)

-- helper pretty functions
formatVar :: T.Text -> T.Text -> IROp -> Doc ann
formatVar dec ident v@(IRVar i _) = 
  (pretty v) <> line 
    <> (pretty dec) <+> (pretty ident) 
      <+> (pretty @T.Text "=") <+> (pretty i)
formatVar dec ident v@(IRConst i _) = 
  (pretty v) <> line 
    <> (pretty dec) <+> (pretty ident) 
      <+> (pretty @T.Text "=") <+> (pretty i)
formatVar dec ident v = 
  (pretty @T.Text dec) <+> (pretty ident) 
    <+> (pretty @T.Text "=") <+> (pretty v)

formatBinary :: L.BinaryOp -> IROp -> IROp -> Doc ann
formatBinary op l@(IRVar li _) r@(IRVar ri _) = 
  (pretty l) <> line <> (pretty r) 
    <> line <> (pretty li) 
      <+> (pretty $ binaryToText op) 
        <+> (pretty ri)
formatBinary op l@(IRConst li _) r@(IRConst ri _) = 
  (pretty l) <> line <> (pretty r) 
    <> line <> (pretty li) 
      <+> (pretty $ binaryToText op) 
        <+> (pretty ri)
formatBinary op l@(IRVar li _) r@(IRConst ri _) = 
  (pretty l) <> line <> (pretty r) 
    <> line <> (pretty li) 
      <+> (pretty $ binaryToText op) 
        <+> (pretty ri)
formatBinary op l@(IRConst li _) r@(IRVar ri _) = 
  (pretty l) <> line <> (pretty r) 
    <> line <> (pretty li) 
      <+> (pretty $ binaryToText op) 
        <+> (pretty ri)
formatBinary op l@(IRVar li _) r = 
  (pretty l) <> line <> (pretty li) 
    <+> (pretty $ binaryToText op) 
      <+> (pretty r)
formatBinary op l@(IRConst li _) r = 
  (pretty l) <> line <> (pretty li) 
    <+> (pretty $ binaryToText op) 
      <+> (pretty r)
formatBinary op l r@(IRVar ri _) = 
  (pretty r) <> line <> (pretty l) 
    <+> (pretty $ binaryToText op) 
      <+> (pretty ri)
formatBinary op l r@(IRConst ri _) = 
  (pretty r) <> line <> (pretty l) 
    <+> (pretty $ binaryToText op) 
      <+> (pretty ri)
formatBinary op l r = 
  (pretty l) 
    <+> (pretty $ binaryToText op) 
      <+> (pretty r)

formatUnary :: L.UnaryOp -> IROp -> Doc ann
formatUnary op v@(IRVar i _) = 
  (pretty v) <> line 
    <> (pretty $ unaryToText op) 
      <+> (pretty i)
formatUnary op v@(IRConst i _) = 
  (pretty v) <> line 
    <> (pretty $ unaryToText op) 
      <+> (pretty i)
formatUnary op v = (pretty @T.Text $ unaryToText op) <+> (pretty v)

formatPrintOrRet :: T.Text -> IROp -> Doc ann
formatPrintOrRet dec v@(IRVar i _) = 
  (pretty v) <> line 
    <> (pretty dec) 
      <+> (pretty i)
formatPrintOrRet dec v@(IRConst i _) = 
  (pretty v) <> line 
    <> (pretty dec) 
      <+> (pretty i)
formatPrintOrRet dec v = (pretty @T.Text dec) <+> (pretty v)

prettifyArg :: L.Arg -> Doc ann
prettifyArg (L.Arg {argIdent=name, argType=ty}) = (pretty @T.Text $ name <> ":") <+> (viaShow ty)

formatIfStmt :: IROp -> Goto -> (Goto, Label) -> Maybe (Goto, Label) -> Doc ann
formatIfStmt cond@(IRVar ident _) gotomain (gototrue, lbl@(Label _)) Nothing =
  (pretty cond) <> line 
    <> (pretty @T.Text "if") 
      <+> (pretty ident) <+> (pretty @T.Text "then goto") 
        <+> (pretty gototrue) <+> (pretty @T.Text "else goto") 
          <+> (pretty gotomain) <> line 
            <> (pretty $ IRLabel lbl)
formatIfStmt cond@(IRConst ident _) gotomain (gototrue, lbl@(Label _)) Nothing =
  (pretty cond) <> line 
    <> (pretty @T.Text "if") <+> (pretty ident) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotomain) 
          <> line <> (pretty $ IRLabel lbl)
formatIfStmt cond gotomain (gototrue, lbl@(Label _)) Nothing =
  (pretty @T.Text "if") <+> (pretty cond) 
    <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
      <+> (pretty @T.Text "else goto") <+> (pretty gotomain) 
        <> line <> (pretty $ IRLabel lbl)
formatIfStmt cond@(IRVar i _) _ (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _))) =
  (pretty cond) <> line 
    <> (pretty @T.Text "if") <+> (pretty i) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
          <> line <> (pretty $ IRLabel tlbl) 
            <+> (pretty $ IRLabel flbl)
formatIfStmt cond@(IRConst i _) _ (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _))) =
  (pretty cond) <> line 
    <> (pretty @T.Text "if") <+> (pretty i) 
      <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
        <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
          <> line <> (pretty $ IRLabel tlbl) 
            <+> (pretty $ IRLabel flbl)
formatIfStmt cond _ (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _))) =
  (pretty @T.Text "if") <+> (pretty cond) 
    <+> (pretty @T.Text "then goto") <+> (pretty gototrue) 
      <+> (pretty @T.Text "else goto") <+> (pretty gotofalse) 
        <> line <> (pretty $ IRLabel tlbl) 
          <+> (pretty $ IRLabel flbl)

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
