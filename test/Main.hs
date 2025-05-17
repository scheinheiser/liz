{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}
module Main (main) where

import qualified Liz.Parser as P

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "Literal parsing" $ do
    it "parse a string" $ do
      parse P.parseStr "" "\"Hello World\"" `shouldParse` "\"Hello World\""

    it "parse a char" $ do
      parse P.parseChar "" "'!'" `shouldParse` "'!'"

    it "parse an integer" $ do
      parse P.parseNum "" "42" `shouldParse` "42"

    it "parse a float" $ do
      parse P.parseNum "" "999.999" `shouldParse` "999.999"

  describe "Variable declaration parsing" $ do
    describe "All type declarations" $ do
      it "parse a string" $ do
        parse P.parseSExpr "" "(var measurement String \"cm\")" `shouldParse` (P.SEVar "measurement" (P.SEType P.String') (P.SELiteral "\"cm\""))

      it "parse a char" $ do
        parse P.parseSExpr "" "(var n Char 'n')" `shouldParse` (P.SEVar "n" (P.SEType P.Char') (P.SELiteral "'n'"))

      it "parse an integer" $ do
        parse P.parseSExpr "" "(const seven Int 7)" `shouldParse` (P.SEConst "seven" (P.SEType P.Int') (P.SELiteral "7"))

      it "parse a float" $ do
        parse P.parseSExpr "" "(const oyler 2.71828)" `shouldParse` (P.SEConst "oyler" (P.SEType P.Float') (P.SELiteral "2.71828"))

      it "parse a boolean value" $ do
        parse P.parseSExpr "" "(var mybool Bool True)" `shouldParse` (P.SEVar "mybool" (P.SEType P.Bool') (P.SELiteral "True"))

      it "parse unit value" $ do
        parse P.parseSExpr "" "(var undef Unit ())" `shouldParse` (P.SEVar "undef" (P.SEType P.Unit') (P.SELiteral "()"))

    describe "Explicit and implicit declaration" $ do
      it "parse a variable with explicit typing" $ do
        parse P.parseSExpr "" "(var hello String \"World\")" `shouldParse` (P.SEVar "hello" (P.SEType P.String') (P.SELiteral "\"World\""))

      it "parse a variable and infer its type" $ do
        parse P.parseSExpr "" "(var four 4)" `shouldParse` (P.SEVar "four" (P.SEType P.Int') (P.SELiteral "4"))

      it "parse a constant with explicit typing" $ do
        parse P.parseSExpr "" "(const tau Float 6.283185)" `shouldParse` (P.SEConst "tau" (P.SEType P.Float') (P.SELiteral "6.283185"))

      it "parse a constant and infer its type" $ do
        parse P.parseSExpr "" "(const pi 3.141592)" `shouldParse` (P.SEConst "pi" (P.SEType P.Float') (P.SELiteral "3.141592"))

      it "parse a nested variable declaration with explicit typing" $ do
        parse P.parseSExpr "" "(var flipped Bool (not True))" `shouldParse` (P.SEVar "flipped" (P.SEType P.Bool') (P.SEUnary P.Not (P.SELiteral "True")))

      it "parse a nested variable declaration and infer its type (should fail)" $ do
        parse P.parseSExpr "" `shouldFailOn` "(var hello_world (+ 5 6))" 

  describe "Function declarations" $ do
    it "parse a function that returns nothing." $ do
      let func = """
        (func does_nothing [] > Unit
        \&  (return ()))\
        \"""
      parse P.parseSExpr "" func `shouldParse` (P.SEFunc P.Func {
        funcIdent = "does_nothing", 
        funcArgs = [], 
        funcReturnType = P.Unit', 
        funcBody = [(P.SEReturn (P.SELiteral "()"))]
      })

    it "parse a function that returns a value." $ do
      let func = """
        (func does_something [] > Unit
        \&  (print "just did something!")
        \&  (return ()))\
        \"""
      parse P.parseSExpr "" func `shouldParse` (P.SEFunc P.Func {
        funcIdent = "does_something", 
        funcArgs = [], 
        funcReturnType = P.Unit', 
        funcBody = [(P.SEPrint (P.SELiteral "\"just did something!\"")),(P.SEReturn (P.SELiteral "()"))]
      })

    it "parse a function with args." $ do
      let func = """
        (func flip [b ~ Bool] > Bool 
        \&  (not b))\
        \"""
      parse P.parseSExpr "" func `shouldParse` (P.SEFunc P.Func {
        funcIdent = "flip",
        funcArgs = [P.Arg {argIdent = "b", argType = P.Bool'}],
        funcReturnType = P.Bool',
        funcBody = [(P.SEUnary (P.Not) (P.SEIdentifier "b"))]
      })
