{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}

module Liz.ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import qualified Liz.Parser as P
import qualified Liz.Common.Types as L

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
        parse P.parseSExpr "" "(var measurement String \"cm\")" `shouldParse` (L.SEVar base L.Var{
          varIdent = "measurement",
          varType = L.String',
          varValue = L.SELiteral (mkPos 1, mkPos 25) "\"cm\""
        })

      it "parse a char" $ do
        parse P.parseSExpr "" "(var n Char 'n')" `shouldParse` (L.SEVar base L.Var{
          varIdent = "n",
          varType = L.Char',
          varValue = L.SELiteral (mkPos 1, mkPos 13) "'n'"
        })

      it "parse an integer" $ do
        parse P.parseSExpr "" "(const seven Int 7)" `shouldParse` (L.SEConst base L.Var{
          varIdent = "seven",
          varType = L.Int',
          varValue = L.SELiteral (mkPos 1, mkPos 18) "7"
        })

      it "parse a float" $ do
        parse P.parseSExpr "" "(const oyler 2.71828)" `shouldParse` (L.SEConst base L.Var{
          varIdent = "oyler",
          varType = L.Float',
          varValue = L.SELiteral (mkPos 1, mkPos 14) "2.71828"
        })

      it "parse a bool value" $ do
        parse P.parseSExpr "" "(var mybool Bool True)" `shouldParse` (L.SEVar base L.Var{
          varIdent = "mybool",
          varType = L.Bool',
          varValue = L.SELiteral (mkPos 1, mkPos 18) "True"
        })

      it "parse a unit value" $ do
        parse P.parseSExpr "" "(var nothing Unit ())" `shouldParse` (L.SEVar base L.Var{
          varIdent = "nothing",
          varType = L.Unit',
          varValue = L.SELiteral (mkPos 1, mkPos 19) "()"
        })

      it "parse an undefined value" $ do
        parse P.parseSExpr "" "(const this_is_undefined Int undefined)" `shouldParse` (L.SEConst base L.Var{
          varIdent = "this_is_undefined",
          varType = L.Int',
          varValue = L.SELiteral (mkPos 1, mkPos 30) "undefined"
        })

    describe "Explicit and implicit declaration" $ do
      it "parse a variable with explicit typing" $ do
        parse P.parseSExpr "" "(var hello String \"World\")" `shouldParse` (L.SEVar base L.Var{
          varIdent = "hello",
          varType = L.String',
          varValue = L.SELiteral (mkPos 1, mkPos 19) "\"World\""
        })

      it "parse a variable and infer its type" $ do
        parse P.parseSExpr "" "(var four 4)" `shouldParse` (L.SEVar base L.Var{
          varIdent = "four",
          varType = L.Int',
          varValue = L.SELiteral (mkPos 1, mkPos 11) "4"
        })

      it "parse a constant with explicit typing" $ do
        parse P.parseSExpr "" "(const tau Float 6.283185)" `shouldParse` (L.SEConst base L.Var{
          varIdent = "tau",
          varType = L.Float',
          varValue = L.SELiteral (mkPos 1, mkPos 18) "6.283185"
        })

      it "parse a constant and infer its type" $ do
        parse P.parseSExpr "" "(const pi 3.141592)" `shouldParse` (L.SEConst base L.Var{
          varIdent = "pi",
          varType = L.Float',
          varValue = L.SELiteral (mkPos 1, mkPos 11) "3.141592"
        })

      it "parse a nested variable declaration with explicit typing" $ do
        parse P.parseSExpr "" "(var flipped Bool (not True))" `shouldParse` (L.SEVar base L.Var{
          varIdent = "flipped",
          varType = L.Bool',
          varValue = L.SEUnary (mkPos 1, mkPos 20) L.Not (L.SELiteral (mkPos 1, mkPos 24) "True")
        })

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
        parse P.parseSExpr "" func `shouldParse` (L.SEFunc L.Func {
          funcIdent = "does_nothing", 
          funcPos = base,
          funcArgs = [], 
          funcReturnType = L.Unit', 
          funcBody = [(L.SEReturn (mkPos 2, mkPos 4) (L.SELiteral (mkPos 2, mkPos 11) "()"))]
        })

      it "parse a function that returns a value." $ do
        let func = """
          (func does_something [] > String
          \&  (const something "just did something!")
          \&  (print something)
          \&  (return something))\
          \"""
        parse P.parseSExpr "" func `shouldParse` (L.SEFunc L.Func {
          funcIdent = "does_something", 
          funcPos = base,
          funcArgs = [], 
          funcReturnType = L.String', 
          funcBody = [(L.SEConst (mkPos 2, mkPos 4) L.Var{varIdent = "something", varType = L.String', varValue = L.SELiteral (mkPos 2, mkPos 20) "\"just did something!\""})
                      ,(L.SEPrint (mkPos 3, mkPos 4) (L.SEIdentifier (mkPos 3, mkPos 10) "something"))
                      ,(L.SEReturn (mkPos 4, mkPos 4) (L.SEIdentifier (mkPos 4, mkPos 11) "something"))]
        })

      it "parse a function with args that returns nothing." $ do
        let func = """
          (func increment_and_print [n ~ Int] > Unit 
          \&  (print (+ 1 n))
          \&  (return ()))\
          \"""
        parse P.parseSExpr "" func `shouldParse` (L.SEFunc L.Func {
          funcIdent = "increment_and_print",
          funcPos = base,
          funcArgs = [L.Arg {argIdent = "n", argType = L.Int'}],
          funcReturnType = L.Unit',
          funcBody = [(L.SEPrint (mkPos 2, mkPos 4) (L.SEBinary (mkPos 2, mkPos 11) (L.Add) (L.SELiteral (mkPos 2, mkPos 13) "1") (L.SEIdentifier (mkPos 2, mkPos 15) "n")))
                     ,(L.SEReturn (mkPos 3, mkPos 4) (L.SELiteral (mkPos 3, mkPos 11) "()"))]
        })

      it "parse a function with args that returns a value." $ do
        let func = """
          (func flip [b ~ Bool] > Bool 
          \&  (return (not b)))\
         \"""
        parse P.parseSExpr "" func `shouldParse` (L.SEFunc L.Func {
          funcIdent = "flip",
          funcPos = base,
          funcArgs = [L.Arg {argIdent = "b", argType = L.Bool'}],
          funcReturnType = L.Bool',
          funcBody = [(L.SEReturn (mkPos 2, mkPos 4) $ L.SEUnary (mkPos 2, mkPos 12) (L.Not) (L.SEIdentifier (mkPos 2, mkPos 16) "b"))]
        })

    describe "Function calls" $ do
      it "parse a function call w/ a literal value" $ do
        parse P.parseSExpr "" "(increment 9)" `shouldParse` (L.SEFuncCall base "increment" [L.SELiteral (mkPos 1, mkPos 12) "9"])

      it "parse a function call w/ a nested expression" $ do
        parse P.parseSExpr "" "(flip (== 10 5))" `shouldParse` (L.SEFuncCall base "flip" [(L.SEBinary (mkPos 1, mkPos 8) L.Eql (L.SELiteral (mkPos 1, mkPos 11) "10") (L.SELiteral (mkPos 1, mkPos 14) "5"))])
