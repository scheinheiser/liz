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

-- to circumvent the checks for a 'main' function in the sema function.
analyseProgram :: L.Program -> Either [E.SemErr] L.Program
analyseProgram p@(L.Program prog) = 
  let
    res = aux prog S.mkSymTbl
    (errs, _) = S.collectErrors res [] []
  in if length errs /= 0 then Left errs
                         else Right p 
  where
    aux :: [L.SExpr] -> S.SymbolTbl -> [Either [E.SemErr] L.Type]
    aux [] _ = []
    aux (ex : exprs) sym =
      let
        (res, next) = S.infer ex sym
      in res : aux exprs next

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
          (S.infer output S.mkSymTbl) `shouldBe` (Left [E.MismatchedTypes base (mkPos 1, mkPos 10) L.Int' "Char"], S.mkSymTbl)

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
        let ntbl = S.addVar L.Var {varIdent="hello_world", varType=L.String', varValue=L.SELiteral "50" (mkPos 1,mkPos 25) (mkPos 1,mkPos 27)} S.mkSymTbl
        (S.infer output S.mkSymTbl) `shouldBe` (Left [E.MismatchedTypes base (mkPos 1, mkPos 27) L.String' "Int"], ntbl)

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
    describe "Function declarations" $ do
      it "Check a function that returns nothing, and has no args." $ do
        let func = """
          (func does_nothing [] > Unit
          \&  (return ()))\
          \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        let ntbl = S.addFunc L.Func {
          funcIdent = "does_nothing", 
          funcStart = base,
          funcEnd = (mkPos 2, mkPos 14),
          funcArgs = [], 
          funcReturnType = L.Unit', 
          funcBody = [(L.SEReturn (mkPos 2, mkPos 4) (mkPos 2, mkPos 13)(L.SELiteral "()" (mkPos 2, mkPos 11) (mkPos 2, mkPos 13)))]
        } S.mkSymTbl
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Unit', ntbl)

      it "Check a function that returns something, and has no args" $ do
        let func = """
          (func does_something [] > String
          \&  (const something "just did something!")
          \&  (print something)
          \&  (return something))\
          \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        let ntbl = S.addFunc L.Func {
          funcIdent = "does_something", 
          funcStart = base,
          funcEnd = (mkPos 4, mkPos 21),
          funcArgs = [], 
          funcReturnType = L.String', 
          funcBody = [(L.SEConst (mkPos 2, mkPos 4) (mkPos 2, mkPos 41) L.Var{varIdent = "something", 
                        varType = L.String', 
                        varValue = L.SELiteral "\"just did something!\"" (mkPos 2, mkPos 20) (mkPos 2, mkPos 41)})
                      ,(L.SEPrint (mkPos 3, mkPos 4) (mkPos 3, mkPos 19) (L.SEIdentifier "something" (mkPos 3, mkPos 10) (mkPos 3, mkPos 19)))
                      ,(L.SEReturn (mkPos 4, mkPos 4) (mkPos 4, mkPos 20)(L.SEIdentifier "something" (mkPos 4, mkPos 11) (mkPos 4, mkPos 20)))]
        } S.mkSymTbl
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.String', ntbl)

      it "Check a function that doesn't return anything, but has args" $ do
        let func = """
          (func increment_and_print [n ~ Int] > Unit 
          \&  (print (+ 1 n))
          \&  (return ()))\
        \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        let ntbl = S.addFunc L.Func {
          funcIdent = "increment_and_print",
          funcStart = base,
          funcEnd = (mkPos 3, mkPos 14),
          funcArgs = [L.Arg {argIdent = "n", argType = L.Int'}],
          funcReturnType = L.Unit',
          funcBody = [(L.SEPrint (mkPos 2, mkPos 4) (mkPos 2, mkPos 17) 
                        (L.SEBinary L.Add (mkPos 2, mkPos 11) (mkPos 2, mkPos 16) (L.SELiteral "1" (mkPos 2, mkPos 13) (mkPos 2, mkPos 14)) (L.SEIdentifier "n" (mkPos 2, mkPos 15) (mkPos 2, mkPos 16))))
                     ,(L.SEReturn (mkPos 3, mkPos 4) (mkPos 3, mkPos 13) (L.SELiteral "()" (mkPos 3, mkPos 11) (mkPos 3, mkPos 13)))]
        } S.mkSymTbl
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Unit', ntbl)

      it "Check a function that returns something, and has args" $ do
        let func = """
          (func flip [b ~ Bool] > Bool 
          \&  (return (not b)))\
        \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        let ntbl = S.addFunc L.Func {
          funcIdent = "flip",
          funcStart = base,
          funcEnd = (mkPos 2, mkPos 19),
          funcArgs = [L.Arg {argIdent = "b", argType = L.Bool'}],
          funcReturnType = L.Bool',
          funcBody = [(L.SEReturn (mkPos 2, mkPos 4) (mkPos 2, mkPos 18) $ L.SEUnary L.Not (mkPos 2, mkPos 12) (mkPos 2, mkPos 17) (L.SEIdentifier "b" (mkPos 2, mkPos 16) (mkPos 2, mkPos 17)))]
        } S.mkSymTbl
        (S.infer output S.mkSymTbl) `shouldBe` (Right L.Bool', ntbl)

      it "Fail checking a function that returns a different type than expected" $ do
        let func = """
          (func decrement [number ~ Int] > Bool
          \&  (return (- number 1)))\
        \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Left [E.MismatchedTypes base (mkPos 2, mkPos 24) L.Bool' "Int"], S.mkSymTbl)

      it "Fail checking a function with an erroneous sexpr within" $ do
        let func = """
          (func does_something [] > String
          \&  (const something String 40)
          \&  (print something)
          \&  (return something))\
          \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        (S.infer output S.mkSymTbl) `shouldBe` (Left [E.MismatchedTypes (mkPos 2,mkPos 4) (mkPos 2,mkPos 29) L.String' "Int"], S.mkSymTbl)
    describe "Function calls" $ do
      it "Check a function call with the correct amount of args" $ do
        let input = """
          (func concat [s1 ~ String, s2 ~ String] > String
          \&  (return (++ s1 s2)))
          (concat "hello " "world")\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Right $ L.Program [(L.SEFunc L.Func {
            funcIdent="concat",
            funcStart=base,
            funcEnd=(mkPos 2, mkPos 22),
            funcArgs=[L.Arg{argIdent="s1", argType=L.String'}, L.Arg{argIdent="s2", argType=L.String'}],
            funcReturnType=L.String',
            funcBody= [
              L.SEReturn (mkPos 2, mkPos 4) (mkPos 2, mkPos 21) 
                (L.SEBinary L.Concat (mkPos 2, mkPos 12) (mkPos 2, mkPos 20) (L.SEIdentifier "s1" (mkPos 2, mkPos 15) (mkPos 2, mkPos 17)) (L.SEIdentifier "s2" (mkPos 2, mkPos 18) (mkPos 2, mkPos 20)))
              ]
         }),
         (L.SEFuncCall (mkPos 3, mkPos 2) (mkPos 3, mkPos 25) "concat" 
          [(L.SELiteral "\"hello \"" (mkPos 3, mkPos 9) (mkPos 3, mkPos 17)), (L.SELiteral "\"world\"" (mkPos 3, mkPos 18) (mkPos 3, mkPos 25))])])

      it "Fail to check a function call with too many args" $ do
        let input = """
          (func concat [s1 ~ String, s2 ~ String] > String
          \&  (return (++ s1 s2)))
          (concat "hello " "world" 50 10)\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Left [E.TooManyArgs (mkPos 3, mkPos 2) (mkPos 3, mkPos 31) "concat" 2])

      it "Fail to check a function call with too little args" $ do
        let input = """
          (func concat [s1 ~ String, s2 ~ String] > String
          \&  (return (++ s1 s2)))
          (concat "won't work...")\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Left [E.NotEnoughArgs (mkPos 3, mkPos 2) (mkPos 3, mkPos 24) "concat" 1])

      it "Fail to check a function call with incorrect arg types" $ do
        let input = """
          (func concat [s1 ~ String, s2 ~ String] > String
          \&  (return (++ s1 s2)))
          (concat True False)\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Left [E.IncorrectArgTypes (mkPos 3, mkPos 2) (mkPos 3, mkPos 19) "concat" [L.String', L.String'] [L.Bool', L.Bool']])
