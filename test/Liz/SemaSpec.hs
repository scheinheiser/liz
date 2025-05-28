{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}

module Liz.SemaSpec (spec) where

import Test.Hspec
import Text.Megaparsec

import qualified Liz.Parser as P
import qualified Liz.Common.Types as L
import qualified Liz.Common.Error as E
import qualified Liz.Sema as S

getOutput :: Either a b -> b
getOutput (Right x) = x

base :: (Pos, Pos)
base = (mkPos 1, mkPos 2)

spec :: Spec
spec = do
  describe "Type checking" $ do
    describe "Basic literals" $ do
      it "Check an int" $ do
        let parsed = parse P.parseNested "" "500"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Int', S.mkSymTbl)

      it "Check a string" $ do
        let parsed = parse P.parseNested "" "\"hello world\""
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.String', S.mkSymTbl)

      it "Check a char" $ do
        let parsed = parse P.parseNested "" "'h'"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Char', S.mkSymTbl)

      it "Check a bool" $ do
        let parsed = parse P.parseNested "" "True"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Bool', S.mkSymTbl)

      it "Check a unit" $ do
        let parsed = parse P.parseNested "" "()"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Unit', S.mkSymTbl)

    describe "Unary expressions" $ do
      it "Check a 'not' expression" $ do
        let parsed = parse P.parseNested "" "(not True)"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Bool', S.mkSymTbl)

      it "Check a 'negate' expression, int" $ do
        let parsed = parse P.parseNested "" "(negate 123)"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Int', S.mkSymTbl)

      it "Check a 'negate' expression, float" $ do
        let parsed = parse P.parseNested "" "(negate 123.0)"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Float', S.mkSymTbl)

      it "Fail checking a 'not' expression with a non-bool value" $ do
        let parsed = parse P.parseNested "" "(not 123.0)"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 11) L.Bool' L.Float'], S.mkSymTbl)

      it "Fail checking a 'negate' expression with a non-numerical value" $ do
        let parsed = parse P.parseNested "" "(negate \"hi world\")"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectTypes base (mkPos 1, mkPos 19) "Float or Int" [L.String']], S.mkSymTbl)

      it "Check a nested unary expression" $ do
        let parsed = parse P.parseNested "" "(not (== True False))"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Bool', S.mkSymTbl)

    describe "Binary expressions" $ do
      describe "Check a 'concat' expression" $ do
        it "Operands are both strings" $ do
          let parsed = parse P.parseNested "" "(++ \"hello \" \"world\")"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Right L.String', S.mkSymTbl)

        it "Left operand is not a string" $ do
          let parsed = parse P.parseNested "" "(++ 50 \"world\")"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 15) L.String' L.Int'], S.mkSymTbl)

        it "Right operand is not a string" $ do
          let parsed = parse P.parseNested "" "(++ \"hello \" True)"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 18) L.String' L.Bool'], S.mkSymTbl)

        it "Both operands aren't strings" $ do
          let parsed = parse P.parseNested "" "(++ 'h' True)"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectTypes base (mkPos 1, mkPos 13) "String" [L.Char', L.Bool']], S.mkSymTbl)

      describe "Check a numerical expression" $ do
        it "Operands are both ints" $ do
          let parsed = parse P.parseNested "" "(+ 5 10)"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Right L.Int', S.mkSymTbl)

        it "Operands are both floats" $ do
          let parsed = parse P.parseNested "" "(/ 5.0 10.0)"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Right L.Float', S.mkSymTbl)

        it "Left operand is non-numerical" $ do
          let parsed = parse P.parseNested "" "(* True 10.0)"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 13) L.Float' L.Bool'], S.mkSymTbl)

        it "Right operand is non-numerical" $ do
          let parsed = parse P.parseNested "" "(- 60 'h')"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 10) L.Int' L.Char'], S.mkSymTbl)

        it "Both operands aren't numerical" $ do
          let parsed = parse P.parseNested "" "(+ \"hello\" 'h')"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectTypes base (mkPos 1, mkPos 15) "Float or Int" [L.String', L.Char']], S.mkSymTbl)

        it "Different numerical types" $ do
          let parsed = parse P.parseNested "" "(+ 60 20.9)"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 11) L.Int' L.Float'], S.mkSymTbl)

      describe "Check a boolean expression" $ do
        it "Operands are both bools" $ do
          let parsed = parse P.parseNested "" "(== True False)"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Right L.Bool', S.mkSymTbl)

        it "Operands are the same type" $ do
          let parsed = parse P.parseNested "" "(< 5 60)"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Right L.Bool', S.mkSymTbl)

        it "Operands aren't the same type" $ do
          let parsed = parse P.parseNested "" "(>= 5 'h')"
          let output = getOutput parsed
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.MismatchedTypes base (mkPos 1, mkPos 10) L.Int' "Char'"], S.mkSymTbl)

      it "Check a nested expression" $ do
        let parsed = parse P.parseNested "" "(== (> 4 5) (not True))"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Bool', S.mkSymTbl)

    describe "Variable declarations" $ do
      it "Check an explicitly-typed variable" $ do
        let parsed = parse P.parseNested "" "(var hello_world String \"hello world\")"
        let output = getOutput parsed
        let ntbl = S.addVar L.Var {varValue=L.SELiteral "\"hello world\"" (mkPos 1, mkPos 25) (mkPos 1, mkPos 38), varType=L.String', varIdent="hello_world"} S.mkSymTbl
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.String', ntbl)

      it "Fail checking an explicitly-typed variable due to the literal-declaration type mismatch" $ do
        let parsed = parse P.parseNested "" "(var hello_world String 50)"
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Left [E.MismatchedTypes base (mkPos 1, mkPos 27) L.String' "Int'"], S.mkSymTbl)

      it "Check an inferred variable declaration" $ do
        let parsed = parse P.parseNested "" "(const unit ())"
        let output = getOutput parsed
        let ntbl = S.addConst L.Var {varValue=L.SELiteral "()" (mkPos 1, mkPos 13) (mkPos 1, mkPos 15), varType=L.Unit', varIdent="unit"} S.mkSymTbl
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Unit', ntbl)

      it "Check a nested variable declaration" $ do
        let parsed = parse P.parseNested "" "(const boolean Bool (== 5 8))"
        let output = getOutput parsed
        let ntbl = S.addConst L.Var {
          varValue=L.SEBinary L.Eql (mkPos 1, mkPos 22) (mkPos 1, mkPos 28) (L.SELiteral "5" (mkPos 1, mkPos 25) (mkPos 1, mkPos 26)) (L.SELiteral "8" (mkPos 1, mkPos 27) (mkPos 1, mkPos 28)), 
          varType=L.Bool', 
          varIdent="boolean"
        } S.mkSymTbl
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Bool', ntbl)
