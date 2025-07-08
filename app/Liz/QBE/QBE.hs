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

data Ext = ExtByte
  | ExtHW

instance Pretty Ext where
  pretty ExtByte = pretty @T.Text "b"
  pretty ExtHW = pretty @T.Text "h"

data Const = CInt Bool Word64 -- flag for 2's complement
  | CSingle Float
  | CFloat Double
  | CGlobal (Ident Global) 

instance Pretty Const where
  pretty (CInt f v) = if f then (pretty @T.Text "-") <> (pretty v) else pretty v
  pretty (CSingle v) = pretty v
  pretty (CFloat v) = pretty v
  pretty (CGlobal i) = pretty i

data Value = VConst Const 
  | VTemp (Ident Temp)

instance Pretty Value where
  pretty (VConst v) = pretty v
  pretty (VTemp i) = pretty i

data VarAssgn = VarAssgn (Ident Temp) Prim

instance Pretty VarAssgn where
  pretty (VarAssgn i t) = (pretty i) <+> (pretty @T.Text "=") <> (pretty t)

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

data MemOp = Alloc16 VarAssgn Value
  | Alloc8 VarAssgn Value
  | Alloc4 VarAssgn Value
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

data Instr = Binary VarAssgn BinOp Value Value
  | Unary VarAssgn UnOp Value
  | Load VarAssgn Prim Value
  | LoadW VarAssgn Sign Value
  | LoadB VarAssgn Sign Value
  | LoadH VarAssgn Sign Value
  | Jump JmpOp
  | Memory MemOp
  | Phi VarAssgn (NE.NonEmpty (Ident Label, Value))

instance Pretty Instr where
  pretty (Binary a op l r) = (pretty a) <+> (pretty op) <+> (pretty l) <+> (pretty r)
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
