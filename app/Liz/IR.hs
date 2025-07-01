{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedDefaults #-}
{-# LANGUAGE OrPatterns #-}

module Liz.IR where

import qualified Liz.Common.Types as L
-- import qualified Liz.Common.Errors as E
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Prettyprinter as PP
import Prettyprinter.Render.Text (putDoc)

getOutput :: Either a b -> b
getOutput (Right x) = x

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

newtype Label = Label (T.Text, [IROp]) -- name, expressions
  deriving Show

type Goto = T.Text

-- helper functions
pushExpr :: Label -> IROp -> Label
pushExpr (Label (n, exprs)) instr = Label (n, instr : exprs)

mkLabel :: Label
mkLabel = Label ("", [])

translateBody :: [L.SExpr] -> Int -> ([IROp], Int)
translateBody l idx = let (res, i) = aux l idx [] in (reverse res, i)
  where
    aux :: [L.SExpr] -> Int -> [IROp] -> ([IROp], Int)
    aux [] i acc = (acc, i)
    aux (x : xs) i acc = let (op, ni) = fromSExpr x i in aux xs ni (op : acc)

tempVarIdent :: Int -> T.Text
tempVarIdent i = T.pack $ "t" <> (show i)

-- main ir functions
fromSExpr :: L.SExpr -> Int -> (IROp, Int)
fromSExpr (L.SELiteral ty lit _ _) i =
  let 
    v = 
      case ty of
        L.Int' -> IRInt $ read @Int (T.unpack lit)
        L.Float' -> IRFloat $ read @Double (T.unpack lit)
        L.Bool' -> IRBool $ read @Bool (T.unpack lit)
        L.Char' -> IRChar $ read @Char (T.unpack lit)
        L.Unit' -> IRUnit
        L.Undef' -> IRUndef
        L.String' -> IRString lit
  in (v, i)
fromSExpr (L.SEBinary op _ _ l r) i =
  let
    (el, ni) = fromSExpr l i
    (er, fi) = fromSExpr r ni
    irOp = 
      case op of
        L.Add -> IRBin L.Add
        L.Subtract -> IRBin L.Subtract
        L.Multiply -> IRBin L.Multiply
        L.Divide -> IRBin L.Divide
        L.Concat -> IRBin L.Concat
        L.GreaterEql -> IRBin L.GreaterEql
        L.LessEql -> IRBin L.LessEql
        L.Eql -> IRBin L.Eql
        L.NotEql -> IRBin L.NotEql
        L.Greater -> IRBin L.Greater
        L.Less -> IRBin L.Less
  in (IRVar (tempVarIdent $ fi + 1) (irOp el er), fi + 1)
fromSExpr (L.SEUnary op _ _ v) i =
  let
    (ev, ni) = fromSExpr v i
    irOp = 
      case op of
        L.Not -> IRUn L.Not
        L.Negate -> IRUn L.Negate
  in (IRVar (tempVarIdent $ ni + 1) (irOp ev), ni + 1)
fromSExpr (L.SEVar _ _ L.Var{varIdent=ident, varValue=val}) i = 
  let (evaluated_value, ni) = fromSExpr val i in ((IRVar ident evaluated_value), ni)
fromSExpr (L.SEConst _ _ L.Var{varIdent=ident, varValue=val}) i = 
  let (evaluated_value, ni) = fromSExpr val i in ((IRConst ident evaluated_value), ni)
fromSExpr (L.SEFunc L.Func{funcIdent=ident, funcArgs=args, funcReturnType=ret, funcBody=body}) i =
  let (tbody, ni) = translateBody body i in (IRFunc ident args tbody ret, ni)
fromSExpr (L.SESet _ _ ident val) i = 
  let (evaluated_value, ni) = fromSExpr val i in ((IRVar ident evaluated_value), ni)
fromSExpr (L.SEIdentifier ident _ _) i = (IRIdent ident, i)
fromSExpr (L.SEReturn _ _ v) i = 
  let (ev, ni) = fromSExpr v i in (IRRet ev, ni)
fromSExpr (L.SEPrint _ _ v) i = 
  let (ev, ni) = fromSExpr v i in (IRPrint ev, ni)
fromSExpr (L.SEIfStmt _ _ cond tbranch Nothing) i =
  let 
    (econd, ni) = fromSExpr cond i
    (etbr, fi) = fromSExpr tbranch ni
    -- no index, as it's assigned in label application to prevent clashing
    label = "L" 
    gotomain = "" -- blank goto because labels haven't been applied to the rest.
  in (IRIf econd gotomain (label, Label (label, [etbr, IRGoto gotomain])) Nothing, fi + 1)
fromSExpr (L.SEIfStmt _ _ cond tbranch (Just fbranch)) i =
  let 
    (econd, ni) = fromSExpr cond i
    (etbr, ti) = fromSExpr tbranch ni
    (efbr, fi) = fromSExpr fbranch ti
    -- no index, as it's assigned in label application to prevent clashing
    label = "L"
    gotomain = "" -- blank goto because labels haven't been applied to the rest.
  in (IRIf econd gotomain (label, Label (label, [etbr, IRGoto gotomain])) (Just (label, Label (label, [efbr, IRGoto gotomain]))), fi + 1)
fromSExpr (L.SEFuncCall _ _ ident vals) i =
  let (eargs, ni) = translateBody vals i in
  (IRFuncCall ident eargs, ni)
fromSExpr (L.SEBlockStmt _ _ vals) i =
  let (ebody, ni) = translateBody vals i in
  (IRBlockStmt ebody, ni)

applyLabels :: NE.NonEmpty IROp -> Int -> ([IROp], Int)
applyLabels prog i = aux (NE.toList prog) (Label ((labelSuffix i), [])) [] i
  where
    labelSuffix :: Int -> T.Text
    labelSuffix index = "L" <> (T.show index)

    aux :: [IROp] -> Label -> [IROp] -> Int -> ([IROp], Int)
    aux [] curr_lbl@(Label (_, exprs)) acc idx = 
      if length exprs == 0 then (acc, idx)
                           else ((IRLabel curr_lbl) : acc, idx)
    aux ((IRFunc ident args fexprs ret) : rest) old_lbl acc idx =
      let 
        (led_body, next_idx) = flip applyLabels idx $ NE.fromList fexprs
        led_func = IRFunc ident args (reverse led_body) ret in
      aux rest (Label (labelSuffix $ next_idx + 1, [])) (led_func : acc) (next_idx + 1)

    aux (IRIf cond gotomain (gototrue, (Label (_, exprs))) Nothing : rest) old_lbl acc idx = 
      let
        inc_idx = idx + 1
        labelled_true = gototrue <> (T.show inc_idx)
        labelled_if = IRIf cond gotomain (labelled_true, (Label (labelled_true, exprs))) Nothing
      in aux rest (Label (labelSuffix $ inc_idx + 1, [])) (labelled_if : (IRLabel old_lbl) : acc) (inc_idx + 1)
    aux (IRIf cond gotomain (gototrue, (Label (_, texprs))) (Just (gotofalse, Label (_, fexprs))) : rest) old_lbl acc idx = 
      let
        labelled_true = gototrue <> (T.show $ idx + 1)
        labelled_false = gotofalse <> (T.show $ idx + 2)
        labelled_if = IRIf cond gotomain (labelled_true, (Label (labelled_true, texprs))) (Just (labelled_false, Label (labelled_false, fexprs)))
      in aux rest (Label (labelSuffix $ idx + 3, [])) (labelled_if : (IRLabel old_lbl) : acc) (idx + 3)

    aux (goto@(IRGoto _) : rest) old_lbl acc idx = aux rest (Label (labelSuffix $ idx + 1, [])) (goto : (IRLabel old_lbl) : acc) (idx + 1)
    aux (expr : rest) curr_lbl acc idx = aux rest (pushExpr curr_lbl expr) acc idx

programToIR :: L.Program -> [IROp]
programToIR (L.Program sexprs) = 
  let 
    (conv, _) = naiveConv sexprs 0 []
    (res, _) = applyLabels (NE.fromList conv) 0
  in res
  where
    naiveConv :: [L.SExpr] -> Int -> [IROp] -> ([IROp], Int)
    naiveConv [] i acc = (acc, i)
    naiveConv (x : xs) i acc = let (instr, ni) = fromSExpr x i in naiveConv xs ni (instr : acc)

ppIR :: L.Program -> IO ()
ppIR prog =
  let progIR = programToIR prog in
  putDoc $ PP.sep $ map prettifyIROp progIR
  where
    prettifyIROp :: IROp -> PP.Doc T.Text
    prettifyIROp (IRInt v) = PP.viaShow v
    prettifyIROp (IRFloat v) = PP.viaShow v
    prettifyIROp (IRBool v) = PP.viaShow v
    prettifyIROp (IRString v) = PP.viaShow v
    prettifyIROp (IRChar v) = PP.viaShow v
    prettifyIROp (IRUndef) = pretty "undefined"
    prettifyIROp (IRUnit) = pretty "()"
    prettifyIROp (IRIdent i) = pretty i
    prettifyIROp (IRBin op l r) = (prettifyIROp l) PP.<+> (pretty $ binaryToText op) PP.<+> (prettifyIROp r)
    prettifyIROp (IRUn op v) = (pretty $ unaryToText op) PP.<+> (prettifyIROp v)
    prettifyIROp (IRRet v) = (pretty "ret") PP.<+> (prettifyIROp v)
    prettifyIROp (IRPrint v) = (pretty "print") PP.<+> (prettifyIROp v)
    prettifyIROp (IRVar i v) = (pretty "var") PP.<+> (pretty i) PP.<+> (pretty "=") PP.<+> (prettifyIROp v)
    prettifyIROp (IRConst i v) = (pretty "const") PP.<+> (pretty i) PP.<+> (pretty "=") PP.<+> (prettifyIROp v)
    prettifyIROp (IRFunc i args body retTy) =
      (pretty i) <> (PP.encloseSep PP.lparen PP.rparen (PP.comma <> PP.space) $ map prettifyArg args) PP.<+> (pretty "->") PP.<+> (PP.viaShow retTy) <> (pretty ":") <> PP.line <> ((PP.indent 2 . PP.vsep) $ map prettifyIROp body)
    prettifyIROp (IRIf cond@(IRVar i _) gotomain (gototrue, lbl@(Label _)) Nothing) =
      (prettifyIROp cond) <> PP.line <> (pretty "if") PP.<+> (pretty i) PP.<+> (pretty "then goto") PP.<+> (pretty gototrue) PP.<+> (pretty "else goto") PP.<+> (pretty gotomain) <> PP.line <> (prettifyIROp $ IRLabel lbl)
    prettifyIROp (IRIf cond@(IRConst i _) gotomain (gototrue, lbl@(Label _)) Nothing) =
      (prettifyIROp cond) <> PP.line <> (pretty "if") PP.<+> (pretty i) PP.<+> (pretty "then goto") PP.<+> (pretty gototrue) PP.<+> (pretty "else goto") PP.<+> (pretty gotomain) <> PP.line <> (prettifyIROp $ IRLabel lbl)
    prettifyIROp (IRIf cond gotomain (gototrue, lbl@(Label _)) Nothing) =
       (pretty "if") PP.<+> (prettifyIROp cond) PP.<+> (pretty "then goto") PP.<+> (pretty gototrue) PP.<+> (pretty "else goto") PP.<+> (pretty gotomain) <> PP.line <> (prettifyIROp $ IRLabel lbl)
    prettifyIROp (IRIf cond@(IRVar i _) gotomain (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _)))) =
      (prettifyIROp cond) <> PP.line <> (pretty "if") PP.<+> (pretty i) PP.<+> (pretty "then goto") PP.<+> (pretty gototrue) PP.<+> (pretty "else goto") PP.<+> (pretty gotofalse) <> PP.line <> (PP.indent 2 ((prettifyIROp $ IRLabel tlbl) PP.<+> (prettifyIROp $ IRLabel flbl)))
    prettifyIROp (IRIf cond@(IRConst i _) gotomain (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _)))) =
      (prettifyIROp cond) <> PP.line <> (pretty "if") PP.<+> (pretty i) PP.<+> (pretty "then goto") PP.<+> (pretty gototrue) PP.<+> (pretty "else goto") PP.<+> (pretty gotofalse) <> PP.line <> (PP.indent 2 ((prettifyIROp $ IRLabel tlbl) PP.<+> (prettifyIROp $ IRLabel flbl)))
    prettifyIROp (IRIf cond gotomain (gototrue, tlbl@(Label _)) (Just (gotofalse, flbl@(Label _)))) =
       (pretty "if") PP.<+> (prettifyIROp cond) PP.<+> (pretty "then goto") PP.<+> (pretty gototrue) PP.<+> (pretty "else goto") PP.<+> (pretty gotofalse) <> PP.line <> (PP.indent 2 ((prettifyIROp $ IRLabel tlbl) PP.<+> (prettifyIROp $ IRLabel flbl)))
    prettifyIROp (IRLabel (Label (name, exprs))) =
      (pretty $ name <> ":") <> PP.line <> ((PP.indent 2 . PP.vcat) $ map prettifyIROp exprs) <> PP.line
    prettifyIROp (IRGoto name) = (pretty "goto") PP.<+> (pretty name)
    prettifyIROp (IRFuncCall i exprs) = 
      (pretty i) <> ((PP.parens . PP.hsep . PP.punctuate PP.comma) $ map prettifyIROp exprs)
    prettifyIROp (IRBlockStmt exprs) = 
      (pretty "block") PP.<+> PP.lbrace <> PP.line <> ((PP.indent 2 . PP.vsep) $ map prettifyIROp exprs) <> PP.line <> PP.rbrace <> PP.line

    prettifyArg :: L.Arg -> PP.Doc T.Text
    prettifyArg (L.Arg {argIdent=name, argType=ty}) = (pretty $ name <> ":") PP.<+> (PP.viaShow ty)

    pretty :: T.Text -> PP.Doc ann
    pretty = PP.pretty @T.Text

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
