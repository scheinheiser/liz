{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}

module Liz.ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import qualified Liz.Parser as P

base :: (Pos, Pos)
base = (mkPos 1, mkPos 2)

spec :: Spec
spec = do
  describe "Literal parsing" $ do
    it "parse a string" $ do
      parse P.parseStr "" "\"Hello World\"" `shouldParse` "\"Hello World\""

    it "parse a char" $ do
      parse P.parseChar "" "'!'" `shouldParse` "'!'"

    it "parse an integer" $ do
      parse P.parseNum "" "42" `shouldParse` "42"

    it "parse a float" $ do
      parse P.parseNum "" "999.999" `shouldParse` "999.999"

    it "parse a unit" $ do
      parse P.parseUnit "" "()" `shouldParse` "()"

    it "parse a bool" $ do
      parse P.parseBool "" "True" `shouldParse` "True"

    it "parse undefined" $ do
      parse P.parseUndefined "" "undefined" `shouldParse` "undefined"

  describe "Variable declaration parsing" $ do
    describe "All type declarations" $ do
      it "parse a string" $ do
        parse P.parseSExpr "" "(var measurement String \"cm\")" `shouldParse` (P.SEVar base "measurement" (P.SEType P.String') (P.SELiteral (mkPos 1, mkPos 25) "\"cm\""))

      it "parse a char" $ do
        parse P.parseSExpr "" "(var n Char 'n')" `shouldParse` (P.SEVar base "n" (P.SEType P.Char') (P.SELiteral (mkPos 1, mkPos 13) "'n'"))

      it "parse an integer" $ do
        parse P.parseSExpr "" "(const seven Int 7)" `shouldParse` (P.SEConst base "seven" (P.SEType P.Int') (P.SELiteral (mkPos 1, mkPos 18) "7"))

      it "parse a float" $ do
        parse P.parseSExpr "" "(const oyler 2.71828)" `shouldParse` (P.SEConst base "oyler" (P.SEType P.Float') (P.SELiteral (mkPos 1, mkPos 14) "2.71828"))

      it "parse a bool value" $ do
        parse P.parseSExpr "" "(var mybool Bool True)" `shouldParse` (P.SEVar base "mybool" (P.SEType P.Bool') (P.SELiteral (mkPos 1, mkPos 18) "True"))

      it "parse a unit value" $ do
        parse P.parseSExpr "" "(var nothing Unit ())" `shouldParse` (P.SEVar base "nothing" (P.SEType P.Unit') (P.SELiteral (mkPos 1, mkPos 19) "()"))

      it "parse an undefined value" $ do
        parse P.parseSExpr "" "(const this_is_undefined Int undefined)" `shouldParse` (P.SEConst base "this_is_undefined" (P.SEType P.Int') (P.SELiteral (mkPos 1, mkPos 30) "undefined"))

    describe "Explicit and implicit declaration" $ do
      it "parse a variable with explicit typing" $ do
        parse P.parseSExpr "" "(var hello String \"World\")" `shouldParse` (P.SEVar base "hello" (P.SEType P.String') (P.SELiteral (mkPos 1, mkPos 19) "\"World\""))

      it "parse a variable and infer its type" $ do
        parse P.parseSExpr "" "(var four 4)" `shouldParse` (P.SEVar base "four" (P.SEType P.Int') (P.SELiteral (mkPos 1, mkPos 11) "4"))

      it "parse a constant with explicit typing" $ do
        parse P.parseSExpr "" "(const tau Float 6.283185)" `shouldParse` (P.SEConst base "tau" (P.SEType P.Float') (P.SELiteral (mkPos 1, mkPos 18) "6.283185"))

      it "parse a constant and infer its type" $ do
        parse P.parseSExpr "" "(const pi 3.141592)" `shouldParse` (P.SEConst base "pi" (P.SEType P.Float') (P.SELiteral (mkPos 1, mkPos 11) "3.141592"))

      it "parse a nested variable declaration with explicit typing" $ do
        parse P.parseSExpr "" "(var flipped Bool (not True))" `shouldParse` (P.SEVar base "flipped" (P.SEType P.Bool') (P.SEUnary (mkPos 1, mkPos 20) P.Not (P.SELiteral (mkPos 1, mkPos 24) "True")))

      it "parse a nested variable declaration and infer its type (should fail)" $ do
        parse P.parseSExpr "" `shouldFailOn` "(var hello_world (+ 5 6))" 

      it "parse an undefined variable declaration and infer its type (should fail)" $ do
        parse P.parseSExpr "" `shouldFailOn` "(var this_wont_work undefined)"

  describe "Function parsing" $ do
    describe "Function declarations" $ do
      it "parse a function that returns nothing." $ do
        let func = """
          (func does_nothing [] > Unit
          \&  (return ()))\
          \"""
        parse P.parseSExpr "" func `shouldParse` (P.SEFunc P.Func {
          funcIdent = "does_nothing", 
          funcPos = base,
          funcArgs = [], 
          funcReturnType = P.Unit', 
          funcBody = [(P.SEReturn (mkPos 2, mkPos 4) (P.SELiteral (mkPos 2, mkPos 11) "()"))]
        })

      it "parse a function that returns a value." $ do
        let func = """
          (func does_something [] > String
          \&  (const something "just did something!")
          \&  (print something)
          \&  (return something))\
          \"""
        parse P.parseSExpr "" func `shouldParse` (P.SEFunc P.Func {
          funcIdent = "does_something", 
          funcPos = base,
          funcArgs = [], 
          funcReturnType = P.String', 
          funcBody = [(P.SEConst (mkPos 2, mkPos 4) "something" (P.SEType P.String') (P.SELiteral (mkPos 2, mkPos 20) "\"just did something!\""))
                      ,(P.SEPrint (mkPos 3, mkPos 4) (P.SEIdentifier "something"))
                      ,(P.SEReturn (mkPos 4, mkPos 4) (P.SEIdentifier "something"))]
        })

      it "parse a function with args that returns nothing." $ do
        let func = """
          (func increment_and_print [n ~ Int] > Unit 
          \&  (print (+ 1 n))
          \&  (return ()))\
          \"""
        parse P.parseSExpr "" func `shouldParse` (P.SEFunc P.Func {
          funcIdent = "increment_and_print",
          funcPos = base,
          funcArgs = [P.Arg {argIdent = "n", argType = P.Int'}],
          funcReturnType = P.Unit',
          funcBody = [(P.SEPrint (mkPos 2, mkPos 4) (P.SEBinary (mkPos 2, mkPos 11) (P.Add) (P.SELiteral (mkPos 2, mkPos 13) "1") (P.SEIdentifier "n")))
                     ,(P.SEReturn (mkPos 3, mkPos 4) (P.SELiteral (mkPos 3, mkPos 11) "()"))]
        })

      it "parse a function with args that returns a value." $ do
        let func = """
          (func flip [b ~ Bool] > Bool 
          \&  (return (not b)))\
         \"""
        parse P.parseSExpr "" func `shouldParse` (P.SEFunc P.Func {
          funcIdent = "flip",
          funcPos = base,
          funcArgs = [P.Arg {argIdent = "b", argType = P.Bool'}],
          funcReturnType = P.Bool',
          funcBody = [(P.SEReturn (mkPos 2, mkPos 4) $ P.SEUnary (mkPos 2, mkPos 12) (P.Not) (P.SEIdentifier "b"))]
        })

    describe "Function calls" $ do
      it "parse a function call w/ a literal value" $ do
        parse P.parseSExpr "" "(increment 9)" `shouldParse` (P.SEFuncCall base "increment" [P.SELiteral (mkPos 1, mkPos 12) "9"])

      it "parse a function call w/ a nested expression" $ do
        parse P.parseSExpr "" "(flip (== 10 5))" `shouldParse` (P.SEFuncCall base "flip" [(P.SEBinary (mkPos 1, mkPos 8) P.Equal (P.SELiteral (mkPos 1, mkPos 11) "10") (P.SELiteral (mkPos 1, mkPos 14) "5"))])
