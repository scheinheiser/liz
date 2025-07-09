{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OrPatterns #-}

module Liz.QBE.QBE where

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

import Prettyprinter
import Data.Word

data Sigil = AT -- aggregate types
  | Global
  | Temp 
  | Label

newtype Ident (t :: Sigil) = Ident T.Text
  deriving (Show, Eq)

instance Pretty (Ident AT) where
  pretty (Ident i) = (pretty @T.Text ":") <> (pretty i)
instance Pretty (Ident Global) where
  pretty (Ident i) = (pretty @T.Text "$") <> (pretty i)
instance Pretty (Ident Temp) where
  pretty (Ident i) = (pretty @T.Text "%") <> (pretty i)
instance Pretty (Ident Label) where
  pretty (Ident i) = (pretty @T.Text "@") <> (pretty i)

data Prim = PrimWord
  | PrimLong
  | PrimSingle
  | PrimDouble
  deriving Eq

instance Pretty Prim where
  pretty PrimWord = pretty @T.Text "w"
  pretty PrimLong = pretty @T.Text "l"
  pretty PrimSingle = pretty @T.Text "s"
  pretty PrimDouble = pretty @T.Text "d"

data Ext = ExtPrim Prim
  | ExtByte
  | ExtHW

instance Pretty Ext where
  pretty (ExtPrim t) = pretty t
  pretty ExtByte = pretty @T.Text "b"
  pretty ExtHW = pretty @T.Text "h"

data AbiTy = AbiPrim Prim
  | AbiAT (Ident AT)

instance Pretty AbiTy where
  pretty (AbiPrim t) = pretty t
  pretty (AbiAT i) = pretty i

data Const = CInt Bool Word64 -- flag for 2's complement
  | CSingle Float
  | CFloat Double
  | CGlobal (Ident Global) 

instance Pretty Const where
  pretty (CInt f v) = if f then (pretty @T.Text "-") <> (pretty v) else pretty v
  pretty (CSingle v) = (pretty @T.Text "s_") <> (pretty v)
  pretty (CFloat v) = (pretty @T.Text "d_") <> (pretty v)
  pretty (CGlobal i) = pretty i

data Value = VConst Const 
  | VTemp (Ident Temp)

instance Pretty Value where
  pretty (VConst v) = pretty v
  pretty (VTemp i) = pretty i

data Assignment = Assignment (Ident Temp) AbiTy

data DataDef = DataDef (Ident Global) (Maybe Int) (NE.NonEmpty DataItem)
data DataItem = DIExt Ext (NE.NonEmpty DataItem)
  | DIString T.Text
  | DIZero Int
  | DIConst Const

instance Pretty DataItem where
  pretty (DIExt t items) = 
    let unpacked_items = NE.toList items in
    (pretty t) <+> (hsep $ map pretty unpacked_items)
  pretty (DIZero am) = (pretty @T.Text "z") <+> (viaShow am)
  pretty (DIConst c) = pretty c
  pretty (DIString s) = (pretty @T.Text "b") <+> (viaShow s) <> comma <+> (pretty @T.Text "b 0")

instance Pretty DataDef where
  pretty (DataDef i Nothing items) =
    let unpacked_items = NE.toList items in
    (pretty @T.Text "data") <+> (pretty i) <+> (pretty @T.Text "=") <+> lbrace <+> (hsep . punctuate comma $ map pretty unpacked_items) <+> rbrace
  pretty (DataDef i (Just alignment) items) =
    let unpacked_items = NE.toList items in
    (pretty @T.Text "data") <+> (pretty i) <+> (pretty @T.Text "=") <+> (pretty @T.Text "align") <+> (viaShow alignment) <+> lbrace <+> (hsep . punctuate comma $ map pretty unpacked_items) <+> rbrace

data Linkage = Export
  | Thread
  | Section T.Text (Maybe [T.Text])

instance Pretty Linkage where
  pretty Export = pretty @T.Text "export"
  pretty Thread = pretty @T.Text "thread"
  pretty (Section name Nothing) = (pretty @T.Text "section") <+> (viaShow name)
  pretty (Section name (Just flags)) = (pretty @T.Text "section") <+> (hsep $ map viaShow (name : flags))

instance Pretty Assignment where
  pretty (Assignment i t) = (pretty i) <+> (pretty @T.Text "=") <> (pretty t)

data BinOp = Add
  | Sub 
  | Div 
  | Mul 
  | UDiv
  | Rem 
  | URem
  | Or 
  | XOr 
  | And
  | Sar 
  | Shr 
  | Shl
  | CEq Prim
  | CNe Prim
  | CSle Prim
  | CSlt Prim
  | CSgt Prim
  | CSge Prim
  | CUle Prim
  | CUlt Prim
  | CUge Prim
  | CUgt Prim
  | CLe Prim
  | CLt Prim
  | CGe Prim
  | CGt Prim
  | CO Prim
  | CUo Prim
  deriving Eq

instance Pretty BinOp where
  pretty Add = pretty @T.Text "add"
  pretty Div = pretty @T.Text "div"
  pretty Mul = pretty @T.Text "mul"
  pretty Sub = pretty @T.Text "sub"
  pretty UDiv = pretty @T.Text "udiv"
  pretty Rem = pretty @T.Text "rem"
  pretty URem = pretty @T.Text "urem"
  pretty Or = pretty @T.Text "or"
  pretty XOr = pretty @T.Text "xor"
  pretty And = pretty @T.Text "and"
  pretty Sar = pretty @T.Text "sar"
  pretty Shr = pretty @T.Text "shr"
  pretty Shl = pretty @T.Text "shl"
  pretty (CEq t)  = (pretty @T.Text "ceq")  <> (pretty t)
  pretty (CNe t)  = (pretty @T.Text "cne")  <> (pretty t)
  pretty (CSle t) = (pretty @T.Text "csle") <> (pretty t)
  pretty (CSlt t) = (pretty @T.Text "cslt") <> (pretty t)
  pretty (CSgt t) = (pretty @T.Text "csgt") <> (pretty t)
  pretty (CSge t) = (pretty @T.Text "csge") <> (pretty t)
  pretty (CUle t) = (pretty @T.Text "cule") <> (pretty t)
  pretty (CUlt t) = (pretty @T.Text "cult") <> (pretty t)
  pretty (CUge t) = (pretty @T.Text "cuge") <> (pretty t)
  pretty (CUgt t) = (pretty @T.Text "cugt") <> (pretty t)
  pretty (CLe t)  = (pretty @T.Text "cle")  <> (pretty t)
  pretty (CLt t)  = (pretty @T.Text "clt")  <> (pretty t)
  pretty (CGe t)  = (pretty @T.Text "cge")  <> (pretty t)
  pretty (CGt t)  = (pretty @T.Text "cgt")  <> (pretty t)
  pretty (CO t)   = (pretty @T.Text "co")   <> (pretty t)
  pretty (CUo t)  = (pretty @T.Text "cuo")  <> (pretty t)

data UnOp = Neg 
  | Dtosi
  | Dtoui
  | Exts
  | Extsb
  | Extsh
  | Extsw
  | Extub
  | Extuh
  | Extuw
  | Sltof
  | Ultof
  | Stosi
  | Stoui
  | Swtof
  | Uwtof
  | Truncd
  | Cast
  | Copy
  deriving Eq

instance Pretty UnOp where
  pretty Neg = pretty @T.Text "neg"
  pretty Dtosi = pretty @T.Text "dtosi"
  pretty Dtoui = pretty @T.Text "dtoui"
  pretty Exts = pretty @T.Text "exts"
  pretty Extsb = pretty @T.Text "extsb"
  pretty Extsh = pretty @T.Text "extsh"
  pretty Extsw = pretty @T.Text "extsw"
  pretty Extub = pretty @T.Text "extub"
  pretty Extuh = pretty @T.Text "extuh"
  pretty Extuw = pretty @T.Text "extuw"
  pretty Sltof = pretty @T.Text "sltof"
  pretty Ultof = pretty @T.Text "ultof"
  pretty Stosi = pretty @T.Text "stosi"
  pretty Stoui = pretty @T.Text "stoui"
  pretty Swtof = pretty @T.Text "swtof"
  pretty Uwtof = pretty @T.Text "uwtof"
  pretty Truncd = pretty @T.Text "truncd"
  pretty Cast = pretty @T.Text "cast"
  pretty Copy = pretty @T.Text "copy"

data JmpOp = Jmp (Ident Label)
  | Jnz Value (Ident Label) (Ident Label)
  | Ret (Maybe Value)
  | Hlt

instance Pretty JmpOp where
  pretty (Jmp i) = (pretty @T.Text "jmp") <+> (pretty i)
  pretty (Jnz v t f)  = (pretty @T.Text "jnz") <+> (hsep . punctuate comma $ [pretty v, pretty t, pretty f])
  pretty (Ret Nothing) = (pretty @T.Text "ret")
  pretty (Ret (Just v)) = (pretty @T.Text "ret") <+> (pretty v)
  pretty Hlt = pretty @T.Text "hlt"

data MemOp = Alloc16 Assignment Value
  | Alloc8 Assignment Value
  | Alloc4 Assignment Value
  | Blit (Ident Temp) (Ident Temp) Const -- ???
  | StoreExt Ext Value (Ident Temp)
  | Store Prim Value (Ident Temp)

instance Pretty MemOp where
  pretty (Alloc16 a v) = (pretty a) <+> (pretty @T.Text "alloc16") <+> (pretty v)
  pretty (Alloc8 a v) = (pretty a) <+>(pretty @T.Text "alloc8") <+> (pretty v)
  pretty (Alloc4 a v) = (pretty a) <+> (pretty @T.Text "alloc4") <+> (pretty v)
  pretty (Blit s d b) = (pretty @T.Text "blit") <+> (pretty s) <+> (pretty d) <+> (pretty b)
  pretty (StoreExt t v i) = (pretty @T.Text "store") <> (pretty t) <+> (pretty v) <> comma <+> (pretty i)
  pretty (Store t v i) = (pretty @T.Text "store") <> (pretty t) <+> (pretty v) <> comma <+> (pretty i)

data Sign = Signed
  | Unsigned

instance Pretty Sign where
  pretty Signed = pretty @T.Text "s"
  pretty Unsigned = pretty @T.Text "u"

data Instr = Binary Assignment BinOp Value Value
  | Unary Assignment UnOp Value
  | Load Assignment Ext Value
  | LoadW Assignment Sign Value
  | LoadB Assignment Sign Value
  | LoadH Assignment Sign Value
  | Jump JmpOp
  | Memory MemOp
  | Phi Assignment (NE.NonEmpty (Ident Label, Value))
  | VAStart (Ident Temp)
  | VAArg Assignment (Ident Temp)
  | Call (Maybe Assignment) (Ident Global) [Param] -- possible assignment, function identifier and arguments

instance Pretty Instr where
  pretty (Binary a op l r) = (pretty a) <+> (pretty op) <+> (pretty l) <> comma <+> (pretty r)
  pretty (Unary a op v) = (pretty a) <+> (pretty op) <+> (pretty v)
  pretty (Load a suffix v) = (pretty a) <+> (pretty @T.Text "load") <> (pretty suffix) <+> (pretty v)
  pretty (LoadW a sign v) = (pretty a) <+> (pretty @T.Text "load") <> (pretty sign) <> (pretty @T.Text "w") <+> (pretty v)
  pretty (LoadB a sign v) = (pretty a) <+> (pretty @T.Text "load") <> (pretty sign) <> (pretty @T.Text "b") <+> (pretty v)
  pretty (LoadH a sign v) = (pretty a) <+> (pretty @T.Text "load") <> (pretty sign) <> (pretty @T.Text "h") <+> (pretty v)
  pretty (Jump jt) = pretty jt
  pretty (Memory mt) = pretty mt
  pretty (Phi a branches) =
    let unpacked_branches = NE.toList branches in
    (pretty a) <+> (pretty @T.Text "phi") <+> (hsep . punctuate comma $ map (\(l, v) -> (pretty l) <+> (pretty v)) unpacked_branches)
  pretty (VAStart i) = (pretty @T.Text "vastart") <+> (pretty i)
  pretty (VAArg a i) = (pretty a) <+> (pretty @T.Text "vaarg") <+> (pretty i)
  pretty (Call Nothing funcIdent params) =
    (pretty @T.Text "call") <+> (pretty funcIdent)
      <> (parens . hsep . punctuate comma $ map pretty params)
  pretty (Call (Just a) funcIdent params) =
    (pretty a) <+> (pretty @T.Text "call") <+> (pretty funcIdent)
      <> (parens . hsep . punctuate comma $ map pretty params)

data Block = Block (Ident Label) [Instr]
instance Pretty Block where
  pretty (Block n exprs) = (pretty n) <> line <> (indent 8 . vsep $ map pretty exprs)

data Param = Regular (Ident Temp) AbiTy
  | Env (Ident Temp)
  | VarMark

instance Pretty Param where
  pretty (Regular i t) = (pretty t) <+> (pretty i)
  pretty (Env i) = (pretty @T.Text "env") <+> (pretty i)
  pretty VarMark = pretty @T.Text "..."

data FuncDef = FuncDef (Maybe Linkage) (Maybe AbiTy) (Ident Global) [Param] (NE.NonEmpty Block)
instance Pretty FuncDef where
  pretty (FuncDef Nothing Nothing i params body) =
    let unpacked_body = NE.toList body in
    (pretty @T.Text "function") <+> (pretty i) 
      <> (parens . hsep . punctuate comma $ map pretty params) 
        <+> lbrace <> line <> (vsep $ map pretty unpacked_body) <> line <> rbrace
  pretty (FuncDef (Just linkage) Nothing i params body) =
    let unpacked_body = NE.toList body in
    (pretty linkage) <+> (pretty @T.Text "function") 
      <+> (pretty i) <> (parens . hsep . punctuate comma $ map pretty params) 
          <+> lbrace <> line <> (vsep $ map pretty unpacked_body) <> line <> rbrace
  pretty (FuncDef Nothing (Just retType) i params body) =
    let unpacked_body = NE.toList body in
    (pretty @T.Text "function") <+> (pretty retType) 
      <+> (pretty i) <> (parens . hsep . punctuate comma $ map pretty params) 
          <+> lbrace <> line <> (vsep $ map pretty unpacked_body) <> line <> rbrace
  pretty (FuncDef (Just linkage) (Just retType) i params body) =
    let unpacked_body = NE.toList body in
    (pretty linkage) <+> (pretty @T.Text "function") 
      <+> (pretty retType) <+> (pretty i) 
        <> (parens . hsep . punctuate comma $ map pretty params) <+> lbrace 
          <> line <> (vsep $ map pretty unpacked_body) <> line <> rbrace
