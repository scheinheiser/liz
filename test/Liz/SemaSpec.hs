{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}

module Liz.SemaSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Data.Map (insert)

import qualified Liz.Parser as P
import qualified Liz.Common.Types as L
import qualified Liz.Common.Errors as E
import qualified Liz.Sema as S

-- convenience and helper functions
getOutput :: Either a b -> b
getOutput (Right x) = x

base :: (Pos, Pos)
base = (mkPos 1, mkPos 2)

addFunc :: L.Func -> S.Env -> S.Env
addFunc (L.Func {funcIdent=ident, funcReturnType=ty, funcArgs=args}) env@(S.Env {envFuncs=fenv}) = env {S.envFuncs = insert ident (ty, (map L.argType args)) fenv}

addVar :: L.Var -> S.Env -> S.Env
addVar (L.Var {varIdent=ident, varType=ty}) env@(S.Env {envVars=venv}) = env {S.envVars = insert ident ty venv}

addConst :: L.Var -> S.Env -> S.Env
addConst (L.Var {varIdent=ident, varType=ty}) env@(S.Env {envConsts=cenv}) = env {S.envConsts = insert ident ty cenv}

analyseProgram :: L.Program -> Either [E.SemErr] L.Program
analyseProgram p@(L.Program prog) = 
  let
    res = aux prog S.mkEnv
    (errs, _) = S.collectErrors res [] []
  in if length errs /= 0 then Left errs
                         else Right p 
  where
    aux :: [L.SExpr] -> S.Env -> [Either [E.SemErr] L.Type]
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
        (S.infer output S.mkEnv) `shouldBe` (Right L.Int', S.mkEnv)

      it "Check a string" $ do
        let parsed = parse P.parseNested "" "\"hello world\""
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.String', S.mkEnv)

      it "Check a char" $ do
        let parsed = parse P.parseNested "" "'h'"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.Char', S.mkEnv)

      it "Check a bool" $ do
        let parsed = parse P.parseNested "" "True"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.Bool', S.mkEnv)

      it "Check a unit" $ do
        let parsed = parse P.parseNested "" "()"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.Unit', S.mkEnv)

    describe "Unary expressions" $ do
      it "Check a 'not' expression" $ do
        let parsed = parse P.parseNested "" "(not True)"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.Bool', S.mkEnv)

      it "Check a 'negate' expression, int" $ do
        let parsed = parse P.parseNested "" "(negate 123)"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.Int', S.mkEnv)

      it "Check a 'negate' expression, float" $ do
        let parsed = parse P.parseNested "" "(negate 123.0)"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.Float', S.mkEnv)

      it "Fail checking a 'not' expression with a non-bool value" $ do
        let parsed = parse P.parseNested "" "(not 123.0)"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 11) L.Bool' L.Float'], S.mkEnv)

      it "Fail checking a 'negate' expression with a non-numerical value" $ do
        let parsed = parse P.parseNested "" "(negate \"hi world\")"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectTypes base (mkPos 1, mkPos 19) "Float or Int" [L.String']], S.mkEnv)

      it "Check a nested unary expression" $ do
        let parsed = parse P.parseNested "" "(not (== True False))"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.Bool', S.mkEnv)

    describe "Binary expressions" $ do
      describe "Check a 'concat' expression" $ do
        it "Operands are both strings" $ do
          let parsed = parse P.parseNested "" "(++ \"hello \" \"world\")"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Right L.String', S.mkEnv)

        it "Left operand is not a string" $ do
          let parsed = parse P.parseNested "" "(++ 50 \"world\")"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 15) L.String' L.Int'], S.mkEnv)

        it "Right operand is not a string" $ do
          let parsed = parse P.parseNested "" "(++ \"hello \" True)"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 18) L.String' L.Bool'], S.mkEnv)

        it "Both operands aren't strings" $ do
          let parsed = parse P.parseNested "" "(++ 'h' True)"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectTypes base (mkPos 1, mkPos 13) "String" [L.Char', L.Bool']], S.mkEnv)

      describe "Check a numerical expression" $ do
        it "Operands are both ints" $ do
          let parsed = parse P.parseNested "" "(+ 5 10)"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Right L.Int', S.mkEnv)

        it "Operands are both floats" $ do
          let parsed = parse P.parseNested "" "(/ 5.0 10.0)"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Right L.Float', S.mkEnv)

        it "Left operand is non-numerical" $ do
          let parsed = parse P.parseNested "" "(* True 10.0)"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 13) L.Float' L.Bool'], S.mkEnv)

        it "Right operand is non-numerical" $ do
          let parsed = parse P.parseNested "" "(- 60 'h')"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 10) L.Int' L.Char'], S.mkEnv)

        it "Both operands aren't numerical" $ do
          let parsed = parse P.parseNested "" "(+ \"hello\" 'h')"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectTypes base (mkPos 1, mkPos 15) "Float or Int" [L.String', L.Char']], S.mkEnv)

        it "Different numerical types" $ do
          let parsed = parse P.parseNested "" "(+ 60 20.9)"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 11) L.Int' L.Float'], S.mkEnv)

      describe "Check a boolean expression" $ do
        it "Operands are both bools" $ do
          let parsed = parse P.parseNested "" "(== True False)"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Right L.Bool', S.mkEnv)

        it "Operands are the same type" $ do
          let parsed = parse P.parseNested "" "(< 5 60)"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Right L.Bool', S.mkEnv)

        it "Operands aren't the same type" $ do
          let parsed = parse P.parseNested "" "(>= 5 'h')"
          let output = getOutput parsed
          (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 10) L.Int' L.Char'], S.mkEnv)

      it "Check a nested expression" $ do
        let parsed = parse P.parseNested "" "(== (> 4 5) (not True))"
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Right L.Bool', S.mkEnv)

    describe "Variable declarations" $ do
      it "Check an explicitly-typed variable" $ do
        let parsed = parse P.parseNested "" "(var hello_world String \"hello world\")"
        let output = getOutput parsed
        let ntbl = addVar L.Var {varValue=L.SELiteral L.String' "\"hello world\"" (mkPos 1, mkPos 25) (mkPos 1, mkPos 38), varType=L.String', varIdent="hello_world"} S.mkEnv
        (S.infer output S.mkEnv) `shouldBe` (Right L.String', ntbl)

      it "Fail checking an explicitly-typed variable due to the literal-declaration type mismatch" $ do
        let parsed = parse P.parseNested "" "(var hello_world String 50)"
        let output = getOutput parsed
        let ntbl = addVar L.Var {varIdent="hello_world", varType=L.String', varValue=L.SELiteral L.Int' "50" (mkPos 1,mkPos 25) (mkPos 1,mkPos 27)} S.mkEnv
        (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 1, mkPos 27) L.String' L.Int'], ntbl)

      it "Check an inferred variable declaration" $ do
        let parsed = parse P.parseNested "" "(const unit ())"
        let output = getOutput parsed
        let ntbl = addConst L.Var {varValue=L.SELiteral L.Unit' "()" (mkPos 1, mkPos 13) (mkPos 1, mkPos 15), varType=L.Unit', varIdent="unit"} S.mkEnv
        (S.infer output S.mkEnv) `shouldBe` (Right L.Unit', ntbl)

      it "Check a nested variable declaration" $ do
        let parsed = parse P.parseNested "" "(const boolean Bool (== 5 8))"
        let output = getOutput parsed
        let ntbl = addConst L.Var {
          varValue=L.SEBinary L.Eql (mkPos 1, mkPos 22) (mkPos 1, mkPos 28) (L.SELiteral L.Int' "5" (mkPos 1, mkPos 25) (mkPos 1, mkPos 26)) (L.SELiteral L.Int' "8" (mkPos 1, mkPos 27) (mkPos 1, mkPos 28)), 
          varType=L.Bool', 
          varIdent="boolean"
        } S.mkEnv
        (S.infer output S.mkEnv) `shouldBe` (Right L.Bool', ntbl)
    describe "Function declarations" $ do
      it "Check a function that returns nothing, and has no args." $ do
        let func = """
          (def does_nothing [] > Unit
          \&  (return ()))\
          \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        let ntbl = addFunc L.Func {
          funcIdent = "does_nothing", 
          funcStart = base,
          funcEnd = (mkPos 2, mkPos 14),
          funcArgs = [], 
          funcReturnType = L.Unit', 
          funcBody = [(L.SEReturn (mkPos 2, mkPos 4) (mkPos 2, mkPos 13)(L.SELiteral L.Unit' "()" (mkPos 2, mkPos 11) (mkPos 2, mkPos 13)))]
        } S.mkEnv
        (S.infer output S.mkEnv) `shouldBe` (Right L.Unit', ntbl)

      it "Check a function that returns something, and has no args" $ do
        let func = """
          (def does_something [] > String
          \&  (const something "just did something!")
          \&  (print something)
          \&  (return something))\
          \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        let ntbl = addFunc L.Func {
          funcIdent = "does_something", 
          funcStart = base,
          funcEnd = (mkPos 4, mkPos 21),
          funcArgs = [], 
          funcReturnType = L.String', 
          funcBody = [(L.SEConst (mkPos 2, mkPos 4) (mkPos 2, mkPos 41) L.Var{varIdent = "something", 
                        varType = L.String', 
                        varValue = L.SELiteral L.String' "\"just did something!\"" (mkPos 2, mkPos 20) (mkPos 2, mkPos 41)})
                      ,(L.SEPrint (mkPos 3, mkPos 4) (mkPos 3, mkPos 19) (L.SEIdentifier "something" (mkPos 3, mkPos 10) (mkPos 3, mkPos 19)))
                      ,(L.SEReturn (mkPos 4, mkPos 4) (mkPos 4, mkPos 20)(L.SEIdentifier "something" (mkPos 4, mkPos 11) (mkPos 4, mkPos 20)))]
        } S.mkEnv
        (S.infer output S.mkEnv) `shouldBe` (Right L.String', ntbl)

      it "Check a function that doesn't return anything, but has args" $ do
        let func = """
          (def increment_and_print [n ~ Int] > Unit 
          \&  (print (+ 1 n))
          \&  (return ()))\
        \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        let ntbl = addFunc L.Func {
          funcIdent = "increment_and_print",
          funcStart = base,
          funcEnd = (mkPos 3, mkPos 14),
          funcArgs = [L.Arg {argIdent = "n", argType = L.Int'}],
          funcReturnType = L.Unit',
          funcBody = [(L.SEPrint (mkPos 2, mkPos 4) (mkPos 2, mkPos 17) 
                        (L.SEBinary L.Add (mkPos 2, mkPos 11) (mkPos 2, mkPos 16) (L.SELiteral L.Int' "1" (mkPos 2, mkPos 13) (mkPos 2, mkPos 14)) (L.SEIdentifier "n" (mkPos 2, mkPos 15) (mkPos 2, mkPos 16))))
                     ,(L.SEReturn (mkPos 3, mkPos 4) (mkPos 3, mkPos 13) (L.SELiteral L.Unit' "()" (mkPos 3, mkPos 11) (mkPos 3, mkPos 13)))]
        } S.mkEnv
        (S.infer output S.mkEnv) `shouldBe` (Right L.Unit', ntbl)

      it "Check a function that returns something, and has args" $ do
        let func = """
          (def flip [b ~ Bool] > Bool 
          \&  (return (not b)))\
        \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        let ntbl = addFunc L.Func {
          funcIdent = "flip",
          funcStart = base,
          funcEnd = (mkPos 2, mkPos 19),
          funcArgs = [L.Arg {argIdent = "b", argType = L.Bool'}],
          funcReturnType = L.Bool',
          funcBody = [(L.SEReturn (mkPos 2, mkPos 4) (mkPos 2, mkPos 18) $ L.SEUnary L.Not (mkPos 2, mkPos 12) (mkPos 2, mkPos 17) (L.SEIdentifier "b" (mkPos 2, mkPos 16) (mkPos 2, mkPos 17)))]
        } S.mkEnv
        (S.infer output S.mkEnv) `shouldBe` (Right L.Bool', ntbl)

      it "Fail checking a function that returns a different type than expected" $ do
        let func = """
          (def decrement [number ~ Int] > Bool
          \&  (return (- number 1)))\
        \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType base (mkPos 2, mkPos 24) L.Bool' L.Int'], S.mkEnv)

      it "Fail checking a function with an erroneous sexpr within" $ do
        let func = """
          (def does_something [] > String
          \&  (const something String 40)
          \&  (print something)
          \&  (return something))\
          \"""
        let parsed = parse P.parseSExpr "" func
        let output = getOutput parsed
        (S.infer output S.mkEnv) `shouldBe` (Left [E.IncorrectType (mkPos 2,mkPos 4) (mkPos 2,mkPos 29) L.String' L.Int'], S.mkEnv)
    describe "Function calls" $ do
      it "Check a function call with the correct amount of args" $ do
        let input = """
          (def concat [s1 ~ String, s2 ~ String] > String
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
          [(L.SELiteral L.String' "hello " (mkPos 3, mkPos 9) (mkPos 3, mkPos 17)), (L.SELiteral L.String' "world" (mkPos 3, mkPos 18) (mkPos 3, mkPos 25))])])

      it "Fail to check a function call with too many args" $ do
        let input = """
          (def concat [s1 ~ String, s2 ~ String] > String
          \&  (return (++ s1 s2)))
          (concat "hello " "world" 50 10)\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Left [E.TooManyArgs (mkPos 3, mkPos 2) (mkPos 3, mkPos 31) "concat" 2])

      it "Fail to check a function call with too little args" $ do
        let input = """
          (def (<>) [s1 ~ String, s2 ~ String] > String
          \&  (return (++ s1 s2)))
          (<> "won't work...")\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Left [E.NotEnoughArgs (mkPos 3, mkPos 2) (mkPos 3, mkPos 20) "<>" 1])

      it "Fail to check a function call with incorrect arg types" $ do
        let input = """
          (def concat [s1 ~ String, s2 ~ String] > String
          \&  (return (++ s1 s2)))
          (concat True False)\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Left [E.IncorrectArgTypes (mkPos 3, mkPos 2) (mkPos 3, mkPos 19) "concat" [L.String', L.String'] [L.Bool', L.Bool']])
    describe "Blocks" $ do
      it "Check a block" $ do
        let input = """
          (var block_assign Int 
          \&  (block 
          \&    (const fiveteen Int (+ 5 10))
          \&    (return (* fiveteen 2))))\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Right $ L.Program [
          L.SEVar (mkPos 1,mkPos 2) (mkPos 4,mkPos 29) (L.Var {
            varIdent = "block_assign", 
            varType = L.Int', 
            varValue = L.SEBlockStmt (mkPos 2,mkPos 4) (mkPos 4,mkPos 28) [
              L.SEConst (mkPos 3,mkPos 6) (mkPos 3,mkPos 33) (L.Var {
                varIdent = "fiveteen", 
                varType = L.Int', 
                varValue = 
                  L.SEBinary L.Add (mkPos 3,mkPos 26) (mkPos 3,mkPos 32) (L.SELiteral L.Int' "5" (mkPos 3,mkPos 28) (mkPos 3,mkPos 29)) (L.SELiteral L.Int' "10" (mkPos 3,mkPos 30) (mkPos 3,mkPos 32))}),
          L.SEReturn (mkPos 4,mkPos 6) (mkPos 4,mkPos 27) 
            (L.SEBinary L.Multiply (mkPos 4,mkPos 14) (mkPos 4,mkPos 26) (L.SEIdentifier "fiveteen" (mkPos 4,mkPos 16) (mkPos 4,mkPos 24)) (L.SELiteral L.Int' "2" (mkPos 4,mkPos 25) (mkPos 4,mkPos 26)))]})])
      it "Fail checking a block" $ do
        let input = """
          (var block_assign String 
          \&  (block 
          \&    (const fiveteen Int (+ 5 10))
          \&    (return (* fiveteen 2))))\
          \"""
        let parsed = P.parseFile "" input
        let output = getOutput parsed
        (analyseProgram output) `shouldBe` (Left [E.IncorrectType (mkPos 1, mkPos 2) (mkPos 4, mkPos 29) L.String' L.Int'])
      describe "If Statements" $ do
        it "Check an if statement with no else" $ do
          let input = """
            (if (== "hello" "hello")
            \&  (print "hello"))\
            \"""
          let parsed = P.parseFile "" input
          let output = getOutput parsed
          (analyseProgram output) `shouldBe` (Right $ L.Program [
            (L.SEIfStmt (mkPos 1, mkPos 2) (mkPos 2, mkPos 18)
              (L.SEBinary L.Eql (mkPos 1, mkPos 6) (mkPos 1, mkPos 24) 
                (L.SELiteral L.String' "hello" (mkPos 1, mkPos 9) (mkPos 1, mkPos 16)) (L.SELiteral L.String' "hello" (mkPos 1, mkPos 17) (mkPos 1, mkPos 24)))
              (L.SEPrint (mkPos 2, mkPos 4) (mkPos 2, mkPos 17) (L.SELiteral L.String' "hello" (mkPos 2, mkPos 10) (mkPos 2, mkPos 17))) Nothing)])
        it "Check an if statement with an else" $ do
          let input = """
            (if (== "hello" "hello")
            \&  (print "hello")
            \&  (print "bye"))\
            \"""
          let parsed = P.parseFile "" input
          let output = getOutput parsed
          (analyseProgram output) `shouldBe` (Right $ L.Program [
            (L.SEIfStmt (mkPos 1, mkPos 2) (mkPos 3, mkPos 16)
            (L.SEBinary L.Eql (mkPos 1, mkPos 6) (mkPos 1, mkPos 24) 
              (L.SELiteral L.String' "hello" (mkPos 1, mkPos 9) (mkPos 1, mkPos 16)) (L.SELiteral L.String' "hello" (mkPos 1, mkPos 17) (mkPos 1, mkPos 24)))
            (L.SEPrint (mkPos 2, mkPos 4) (mkPos 2, mkPos 17) (L.SELiteral L.String' "hello" (mkPos 2, mkPos 10) (mkPos 2, mkPos 17)))
            (Just (L.SEPrint (mkPos 3, mkPos 4) (mkPos 3, mkPos 15) (L.SELiteral L.String' "bye" (mkPos 3, mkPos 10) (mkPos 3, mkPos 15)))))])
        it "Fail checking an if statement with a branch type mismatch" $ do
          let input = """
            (if (not True)
            \&  (return "this can't work...")
            \&  (print "bye"))\
            \"""
          let parsed = P.parseFile "" input
          let output = getOutput parsed
          (analyseProgram output) `shouldBe` (Left [E.IncorrectType (mkPos 1, mkPos 2) (mkPos 3, mkPos 16) L.String' L.Unit'])
        it "Fail checking an if statement with a non-bool condition" $ do
          let input = """
            (if (+ 5 10)
            \&  (return "this can't work...")
            \&  (print "bye"))\
            \"""
          let parsed = P.parseFile "" input
          let output = getOutput parsed
          (analyseProgram output) `shouldBe` (Left [(E.IncorrectType (mkPos 1, mkPos 2) (mkPos 3, mkPos 16) L.Bool' L.Int'), (E.IncorrectType (mkPos 1, mkPos 2) (mkPos 3, mkPos 16) L.String' L.Unit')])
        it "Fail checking an if statement with an erroneous sexpr" $ do
          let input = """
            (if (not True)
            \&  (block 
            \&    (var five String 10)
            \&    (return five))
            \&  (return 10))\
            \"""
          let parsed = P.parseFile "" input
          let output = getOutput parsed
          (analyseProgram output) `shouldBe` (Left [E.IncorrectType (mkPos 3, mkPos 6) (mkPos 3, mkPos 24) L.String' L.Int'])
      describe "Macros" $ do
        it "Fail checking a macro local to a function." $ do
          let input = """
            (def main [] > Unit 
            \&  (macro hello-world "hello world!")
            \&  (print %hello-world))\
          \"""
          let parsed = P.parseFile "" input
          let output = getOutput parsed
          (S.analyseProgram output) `shouldBe` (Left [E.NonGlblMacroDef (mkPos 1, mkPos 2) (mkPos 3, mkPos 23)])
        it "Fail checking a recursive macro definition." $ do
          let input = """
            (macro hello-world (block 
            \&  (print "hello, world!")
            \&  (return %hello-world)))
            (def main [] > Unit 
            \&  (return %hello-world))\
          \"""
          let parsed = P.parseFile "" input
          let output = getOutput parsed
          (S.analyseProgram output) `shouldBe` (Left [E.RecursiveMacroDef (mkPos 1, mkPos 2) (mkPos 3, mkPos 25) "hello-world"])
        it "Check a program with an expanded macro." $ do
          let input = """
            (macro hello-world "hello world!")
            (def main [] > Unit 
            \&  (print %hello-world))\
          \"""
          let parsed = P.parseFile "" input
          let output = getOutput parsed
          (S.analyseProgram output) `shouldBe` (Right $ L.Program [L.SEFunc $ L.Func {
              funcIdent = "main",
              funcStart = (mkPos 2, mkPos 2),
              funcEnd = (mkPos 3, mkPos 23),
              funcArgs = [],
              funcReturnType = L.Unit',
              funcBody = [(L.SEExpr $ L.EPrint (mkPos 3, mkPos 4) (mkPos 3, mkPos 22) $ L.SEExpr $ L.ELiteral L.String' "hello world!" (mkPos 1, mkPos 20) (mkPos 1, mkPos 34))]
            }])
