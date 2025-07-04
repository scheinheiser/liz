{-# LANGUAGE OverloadedStrings #-}

module Liz.IR.IRTypes (IR (..), IROp (..), Label (..), Goto) where

import qualified Liz.Common.Types as L
import qualified Prettyprinter as PP
import qualified Data.Text as T

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
  | IRIf IROp Goto (Goto, Label) (Maybe (Goto, Label)) -- cond - true branch - optional false branch
  | IRLabel Label -- a wrapper around the label for the leader algorithm
  | IRGoto Goto -- a wrapper around goto for control flow/leader algorithm
  deriving Show

type Goto = T.Text
newtype Label = Label (T.Text, [IROp]) -- name, expressions
  deriving Show

instance PP.Pretty IROp where
  pretty (IRInt v) = PP.viaShow v
  pretty (IRFloat v) = PP.viaShow v
  pretty (IRBool v) = PP.viaShow v
  pretty (IRString v) = (PP.pretty @T.Text "string[") <> (PP.pretty v) <> (PP.pretty @T.Text "]")
  pretty (IRChar v) = PP.viaShow v
  pretty (IRUndef) = PP.pretty @T.Text "undefined"
  pretty (IRUnit) = PP.pretty @T.Text "()"
  pretty (IRIdent i) = PP.pretty @T.Text i
  pretty (IRBin op l r) = formatBinary op l r
  pretty (IRUn op v) = formatUnary op v
  pretty (IRRet v) = formatPrintOrRet "ret" v
  pretty (IRPrint v) = formatPrintOrRet "print" v
  pretty (IRVar i v) = formatVar "var" i v
  pretty (IRConst i v) = formatVar "const" i v
  pretty (IRFunc i args body retTy) =
    (PP.pretty @T.Text i) <> (PP.encloseSep PP.lparen PP.rparen (PP.comma <> PP.space) $ map prettifyArg args) PP.<+> (PP.pretty @T.Text "->") PP.<+> (PP.viaShow retTy) <> (PP.pretty @T.Text ":") <> PP.line <> ((PP.indent 2 . PP.vcat) $ map PP.pretty body)
  pretty (IRIf cond gotomain truebranch falsebranch) = formatIfStmt cond gotomain truebranch falsebranch
  pretty (IRLabel (Label (name, exprs))) =
    (PP.pretty @T.Text $ name <> ":") <> PP.line <> ((PP.indent 2 . PP.vcat) $ map PP.pretty exprs) <> PP.line
  pretty (IRGoto name) = (PP.pretty @T.Text "goto") PP.<+> (PP.pretty name)
  pretty (IRFuncCall i exprs) = 
    (PP.pretty @T.Text i) <> ((PP.parens . PP.hsep . PP.punctuate PP.comma) $ map PP.pretty exprs)
  pretty (IRBlockStmt exprs) = 
    (PP.pretty @T.Text "block:") <> PP.line <> ((PP.indent 2 . PP.vcat) $ map PP.pretty exprs)

-- helper pretty functions
formatVar :: T.Text -> T.Text -> IROp -> PP.Doc ann
formatVar dec ident v@(IRVar i _) = (PP.pretty v) <> PP.line <> (PP.pretty dec) PP.<+> (PP.pretty ident) PP.<+> (PP.pretty @T.Text "=") PP.<+> (PP.pretty i)
formatVar dec ident v@(IRConst i _) = (PP.pretty v) <> PP.line <> (PP.pretty dec) PP.<+> (PP.pretty ident) PP.<+> (PP.pretty @T.Text "=") PP.<+> (PP.pretty i)
formatVar dec ident v = (PP.pretty @T.Text dec) PP.<+> (PP.pretty ident) PP.<+> (PP.pretty @T.Text "=") PP.<+> (PP.pretty v)

formatBinary :: L.BinaryOp -> IROp -> IROp -> PP.Doc ann
formatBinary op l@(IRVar li _) r@(IRVar ri _) = (PP.pretty l) <> PP.line <> (PP.pretty r) <> PP.line <> (PP.pretty li) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty ri)
formatBinary op l@(IRConst li _) r@(IRConst ri _) = (PP.pretty l) <> PP.line <> (PP.pretty r) <> PP.line <> (PP.pretty li) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty ri)
formatBinary op l@(IRVar li _) r@(IRConst ri _) = (PP.pretty l) <> PP.line <> (PP.pretty r) <> PP.line <> (PP.pretty li) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty ri)
formatBinary op l@(IRConst li _) r@(IRVar ri _) = (PP.pretty l) <> PP.line <> (PP.pretty r) <> PP.line <> (PP.pretty li) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty ri)
formatBinary op l@(IRVar li _) r = (PP.pretty l) <> PP.line <> (PP.pretty li) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty r)
formatBinary op l@(IRConst li _) r = (PP.pretty l) <> PP.line <> (PP.pretty li) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty r)
formatBinary op l r@(IRVar ri _) = (PP.pretty r) <> PP.line <> (PP.pretty l) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty ri)
formatBinary op l r@(IRConst ri _) = (PP.pretty r) <> PP.line <> (PP.pretty l) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty ri)
formatBinary op l r = (PP.pretty l) PP.<+> (PP.pretty $ binaryToText op) PP.<+> (PP.pretty r)

formatUnary :: L.UnaryOp -> IROp -> PP.Doc ann
formatUnary op v@(IRVar i _) = (PP.pretty v) <> PP.line <> (PP.pretty $ unaryToText op) PP.<+> (PP.pretty i)
formatUnary op v@(IRConst i _) = (PP.pretty v) <> PP.line <> (PP.pretty $ unaryToText op) PP.<+> (PP.pretty i)
formatUnary op v = (PP.pretty @T.Text $ unaryToText op) PP.<+> (PP.pretty v)

formatPrintOrRet :: T.Text -> IROp -> PP.Doc ann
formatPrintOrRet dec v@(IRVar i _) = (PP.pretty v) <> PP.line <> (PP.pretty dec) PP.<+> (PP.pretty i)
formatPrintOrRet dec v@(IRConst i _) = (PP.pretty v) <> PP.line <> (PP.pretty dec) PP.<+> (PP.pretty i)
formatPrintOrRet dec v = (PP.pretty @T.Text dec) PP.<+> (PP.pretty v)

prettifyArg :: L.Arg -> PP.Doc ann
prettifyArg (L.Arg {argIdent=name, argType=ty}) = (PP.pretty @T.Text $ name <> ":") PP.<+> (PP.viaShow ty)

formatIfStmt :: IROp -> Goto -> (Goto, Label) -> Maybe (Goto, Label) -> PP.Doc ann
formatIfStmt cond@(IRVar ident _) gotomain (gototrue, lbl@(Label _)) Nothing =
  (PP.pretty cond) <> PP.line <> (PP.pretty @T.Text "if") PP.<+> (PP.pretty ident) PP.<+> (PP.pretty @T.Text "then goto") PP.<+> (PP.pretty gototrue) PP.<+> (PP.pretty @T.Text "else goto") PP.<+> (PP.pretty gotomain) <> PP.line <> (PP.pretty $ IRLabel lbl)
formatIfStmt cond@(IRConst ident _) gotomain (gototrue, lbl@(Label _)) Nothing =
  (PP.pretty cond) <> PP.line <> (PP.pretty @T.Text "if") PP.<+> (PP.pretty ident) PP.<+> (PP.pretty @T.Text "then goto") PP.<+> (PP.pretty gototrue) PP.<+> (PP.pretty @T.Text "else goto") PP.<+> (PP.pretty gotomain) <> PP.line <> (PP.pretty $ IRLabel lbl)
formatIfStmt cond gotomain (gototrue, lbl@(Label _)) Nothing =
  (PP.pretty @T.Text "if") PP.<+> (PP.pretty cond) PP.<+> (PP.pretty @T.Text "then goto") PP.<+> (PP.pretty gototrue) PP.<+> (PP.pretty @T.Text "else goto") PP.<+> (PP.pretty gotomain) <> PP.line <> (PP.pretty $ IRLabel lbl)
formatIfStmt cond@(IRVar i _) _ (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _))) =
  (PP.pretty cond) <> PP.line <> (PP.pretty @T.Text "if") PP.<+> (PP.pretty i) PP.<+> (PP.pretty @T.Text "then goto") PP.<+> (PP.pretty gototrue) PP.<+> (PP.pretty @T.Text "else goto") PP.<+> (PP.pretty gotofalse) <> PP.line <> (PP.pretty $ IRLabel tlbl) PP.<+> (PP.pretty $ IRLabel flbl)
formatIfStmt cond@(IRConst i _) _ (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _))) =
  (PP.pretty cond) <> PP.line <> (PP.pretty @T.Text "if") PP.<+> (PP.pretty i) PP.<+> (PP.pretty @T.Text "then goto") PP.<+> (PP.pretty gototrue) PP.<+> (PP.pretty @T.Text "else goto") PP.<+> (PP.pretty gotofalse) <> PP.line <> (PP.pretty $ IRLabel tlbl) PP.<+> (PP.pretty $ IRLabel flbl)
formatIfStmt cond _ (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _))) =
  (PP.pretty @T.Text "if") PP.<+> (PP.pretty cond) PP.<+> (PP.pretty @T.Text "then goto") PP.<+> (PP.pretty gototrue) PP.<+> (PP.pretty @T.Text "else goto") PP.<+> (PP.pretty gotofalse) <> PP.line <> (PP.pretty $ IRLabel tlbl) PP.<+> (PP.pretty $ IRLabel flbl)

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
