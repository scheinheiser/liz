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

-- TODO: tests for if/block parsing.
spec :: Spec
spec = do
  describe "Literal parsing" $ do
    it "parse a string" $ do
      parse P.parseStr "" "\"Hello World\"" `shouldParse` "Hello World"

    it "parse a char" $ do
      parse P.parseChar "" "'!'" `shouldParse` "!"

    it "parse a unit" $ do
      parse P.parseUnit "" "()" `shouldParse` "()"

    it "parse a bool" $ do
      parse P.parseBool "" "True" `shouldParse` "True"

  describe "Variable declaration parsing" $ do
    describe "All type declarations" $ do
      it "parse a string" $ do
        parse P.parseSExpr "" "(var measurement String \"cm\")" `shouldParse` (L.SEVar base (mkPos 1, mkPos 29) L.Var{
          varIdent = "measurement",
          varType = L.String',
          varValue = L.SELiteral L.String' "cm" (mkPos 1, mkPos 25) (mkPos 1, mkPos 29)
        })

      it "parse a char" $ do
        parse P.parseSExpr "" "(var n Char 'n')" `shouldParse` (L.SEVar base (mkPos 1, mkPos 16) L.Var{
          varIdent = "n",
          varType = L.Char',
          varValue = L.SELiteral L.Char' "n" (mkPos 1, mkPos 13) (mkPos 1, mkPos 16)
        })

      it "parse an integer" $ do
        parse P.parseSExpr "" "(const seven Int 7)" `shouldParse` (L.SEConst base (mkPos 1, mkPos 19) L.Var{
          varIdent = "seven",
          varType = L.Int',
          varValue = L.SELiteral L.Int' "7" (mkPos 1, mkPos 18) (mkPos 1, mkPos 19)
        })

      it "parse a float" $ do
        parse P.parseSExpr "" "(const oyler 2.71828)" `shouldParse` (L.SEConst base (mkPos 1, mkPos 21) L.Var{
          varIdent = "oyler",
          varType = L.Float',
          varValue = L.SELiteral L.Float' "2.71828" (mkPos 1, mkPos 14) (mkPos 1, mkPos 21)
        })

      it "parse a bool value" $ do
        parse P.parseSExpr "" "(var mybool Bool True)" `shouldParse` (L.SEVar base (mkPos 1, mkPos 22) L.Var{
          varIdent = "mybool",
          varType = L.Bool',
          varValue = L.SELiteral L.Bool' "True" (mkPos 1, mkPos 18) (mkPos 1, mkPos 22)
        })

      it "parse a unit value" $ do
        parse P.parseSExpr "" "(var nothing Unit ())" `shouldParse` (L.SEVar base (mkPos 1, mkPos 21) L.Var{
          varIdent = "nothing",
          varType = L.Unit',
          varValue = L.SELiteral L.Unit' "()" (mkPos 1, mkPos 19) (mkPos 1, mkPos 21)
        })

    describe "Explicit and implicit declaration" $ do
      it "parse a variable with explicit typing" $ do
        parse P.parseSExpr "" "(var hello String \"World\")" `shouldParse` (L.SEVar base (mkPos 1, mkPos 26) L.Var{
          varIdent = "hello",
          varType = L.String',
          varValue = L.SELiteral L.String' "World" (mkPos 1, mkPos 19) (mkPos 1, mkPos 26)
        })

      it "parse a variable and infer its type" $ do
        parse P.parseSExpr "" "(const pi 3.141592)" `shouldParse` (L.SEConst base (mkPos 1, mkPos 19) L.Var{
          varIdent = "pi",
          varType = L.Float',
          varValue = L.SELiteral L.Float' "3.141592" (mkPos 1, mkPos 11) (mkPos 1, mkPos 19)
        })

      it "parse a nested variable declaration with explicit typing" $ do
        parse P.parseSExpr "" "(var flipped Bool (not True))" `shouldParse` (L.SEVar base (mkPos 1, mkPos 29) L.Var{
          varIdent = "flipped",
          varType = L.Bool',
          varValue = L.SEUnary L.Not (mkPos 1, mkPos 20) (mkPos 1, mkPos 28) (L.SELiteral L.Bool' "True" (mkPos 1, mkPos 24) (mkPos 1, mkPos 28))
        })

      it "parse a nested variable declaration and infer its type (should fail)" $ do
        parse P.parseSExpr "" `shouldFailOn` "(var hello_world (+ 5 6))" 

  describe "Expression parsing" $ do
    describe "Function declarations" $ do
      it "parse a function that returns nothing." $ do
        let func = """
          (def does_nothing [] > Unit
          \&  (return ()))\
          \"""
        parse P.parseSExpr "" func `shouldParse` (L.SEFunc L.Func {
          funcIdent = "does_nothing", 
          funcStart = base,
          funcEnd = (mkPos 2, mkPos 14),
          funcArgs = [], 
          funcReturnType = L.Unit', 
          funcBody = [(L.SEReturn (mkPos 2, mkPos 4) (mkPos 2, mkPos 13)(L.SELiteral L.Unit' "()" (mkPos 2, mkPos 11) (mkPos 2, mkPos 13)))]
        })

      it "parse a function that returns a value." $ do
        let func = """
          (def does_something [] > String
          \&  (const something "just did something!")
          \&  (print something)
          \&  (return something))\
          \"""
        parse P.parseSExpr "" func `shouldParse` (L.SEFunc L.Func {
          funcIdent = "does_something", 
          funcStart = base,
          funcEnd = (mkPos 4, mkPos 21),
          funcArgs = [], 
          funcReturnType = L.String', 
          funcBody = [(L.SEConst (mkPos 2, mkPos 4) (mkPos 2, mkPos 41) L.Var{varIdent = "something", 
                        varType = L.String', 
                        varValue = L.SELiteral L.String' "just did something!" (mkPos 2, mkPos 20) (mkPos 2, mkPos 41)})
                      ,(L.SEPrint (mkPos 3, mkPos 4) (mkPos 3, mkPos 19) (L.SEIdentifier "something" (mkPos 3, mkPos 10) (mkPos 3, mkPos 19)))
                      ,(L.SEReturn (mkPos 4, mkPos 4) (mkPos 4, mkPos 20)(L.SEIdentifier "something" (mkPos 4, mkPos 11) (mkPos 4, mkPos 20)))]
        })

      it "parse a function with args that returns nothing." $ do
        let func = """
          (def increment_and_print [n ~ Int] > Unit 
          \&  (print (+ 1 n))
          \&  (return ()))\
          \"""
        parse P.parseSExpr "" func `shouldParse` (L.SEFunc L.Func {
          funcIdent = "increment_and_print",
          funcStart = base,
          funcEnd = (mkPos 3, mkPos 14),
          funcArgs = [L.Arg {argIdent = "n", argType = L.Int'}],
          funcReturnType = L.Unit',
          funcBody = [(L.SEPrint (mkPos 2, mkPos 4) (mkPos 2, mkPos 17) 
                        (L.SEBinary L.Add (mkPos 2, mkPos 11) (mkPos 2, mkPos 16) (L.SELiteral L.Int' "1" (mkPos 2, mkPos 13) (mkPos 2, mkPos 14)) (L.SEIdentifier "n" (mkPos 2, mkPos 15) (mkPos 2, mkPos 16))))
                     ,(L.SEReturn (mkPos 3, mkPos 4) (mkPos 3, mkPos 13) (L.SELiteral L.Unit' "()" (mkPos 3, mkPos 11) (mkPos 3, mkPos 13)))]
        })

      it "parse a function with args that returns a value." $ do
        let func = """
          (def flip [b ~ Bool] > Bool 
          \&  (return (not b)))\
         \"""
        parse P.parseSExpr "" func `shouldParse` (L.SEFunc L.Func {
          funcIdent = "flip",
          funcStart = base,
          funcEnd = (mkPos 2, mkPos 19),
          funcArgs = [L.Arg {argIdent = "b", argType = L.Bool'}],
          funcReturnType = L.Bool',
          funcBody = [(L.SEReturn (mkPos 2, mkPos 4) (mkPos 2, mkPos 18) $ L.SEUnary L.Not (mkPos 2, mkPos 12) (mkPos 2, mkPos 17) (L.SEIdentifier "b" (mkPos 2, mkPos 16) (mkPos 2, mkPos 17)))]
        })

    describe "Function calls" $ do
      it "parse a function call w/ a literal value" $ do
        parse P.parseSExpr "" "(increment 9)" `shouldParse` (L.SEFuncCall base (mkPos 1, mkPos 13) "increment" [L.SELiteral L.Int' "9" (mkPos 1, mkPos 12) (mkPos 1, mkPos 13)])

      it "parse a function call w/ a nested expression" $ do
        parse P.parseSExpr "" "(flip (== 10 5))" `shouldParse` (L.SEFuncCall base (mkPos 1, mkPos 16) "flip" [(L.SEBinary L.Eql (mkPos 1, mkPos 8) (mkPos 1, mkPos 15) (L.SELiteral L.Int' "10" (mkPos 1, mkPos 11) (mkPos 1, mkPos 13)) (L.SELiteral L.Int' "5" (mkPos 1, mkPos 14) (mkPos 1, mkPos 15)))])

    describe "Block Statement" $ do
      it "parse a block statement" $ do
        let block = """
          (block
          \&  (const something "just did something!")
          \&  (print something)
          \&  (return something))\
          \"""
        parse P.parseSExpr "" block `shouldParse` (L.SEBlockStmt 
          (mkPos 1, mkPos 2) 
          (mkPos 4, mkPos 21) 
          [(L.SEConst (mkPos 2, mkPos 4) (mkPos 2, mkPos 41) L.Var{varIdent = "something", varType = L.String', varValue = L.SELiteral L.String' "just did something!" (mkPos 2, mkPos 20) (mkPos 2, mkPos 41)})
            ,(L.SEPrint (mkPos 3, mkPos 4) (mkPos 3, mkPos 19) (L.SEIdentifier "something" (mkPos 3, mkPos 10) (mkPos 3, mkPos 19)))
            ,(L.SEReturn (mkPos 4, mkPos 4) (mkPos 4, mkPos 20)(L.SEIdentifier "something" (mkPos 4, mkPos 11) (mkPos 4, mkPos 20)))])

    describe "If Statement" $ do
      it "parse an if statement without an else branch" $ do
        let ifstmt = """
          (if (> 10 5)
          \&  (print "10 is greater than 5"))\
          \"""
        parse P.parseSExpr "" ifstmt `shouldParse` (L.SEIfStmt
          (mkPos 1, mkPos 2)
          (mkPos 2, mkPos 33) 
          (L.SEBinary L.Greater (mkPos 1, mkPos 6) (mkPos 1, mkPos 12) (L.SELiteral L.Int' "10" (mkPos 1, mkPos 8) (mkPos 1, mkPos 10)) (L.SELiteral L.Int' "5" (mkPos 1, mkPos 11) (mkPos 1, mkPos 12)))
          (L.SEPrint (mkPos 2, mkPos 4) (mkPos 2, mkPos 32) (L.SELiteral L.String' "10 is greater than 5" (mkPos 2, mkPos 10) (mkPos 2, mkPos 32)))
          Nothing)
      it "parse an if statement with an else branch" $ do
        let ifstmt = """
          (if (> 10 5)
          \&  (print "10 is greater than 5")
          \&  (print "10 isn't greater than 5"))\
          \"""
        parse P.parseSExpr "" ifstmt `shouldParse` (L.SEIfStmt
          (mkPos 1, mkPos 2)
          (mkPos 3, mkPos 36) 
          (L.SEBinary L.Greater (mkPos 1, mkPos 6) (mkPos 1, mkPos 12) (L.SELiteral L.Int' "10" (mkPos 1, mkPos 8) (mkPos 1, mkPos 10)) (L.SELiteral L.Int' "5" (mkPos 1, mkPos 11) (mkPos 1, mkPos 12)))
          (L.SEPrint (mkPos 2, mkPos 4) (mkPos 2, mkPos 32) (L.SELiteral L.String' "10 is greater than 5" (mkPos 2, mkPos 10) (mkPos 2, mkPos 32)))
          (Just ((L.SEPrint (mkPos 3, mkPos 4) (mkPos 3, mkPos 35) (L.SELiteral L.String' "10 isn't greater than 5" (mkPos 3, mkPos 10) (mkPos 3, mkPos 35))))))
    describe "Macros" $ do
      it "parse a macro declaration" $ do
        let input = "(macro boom (print \"explode\"))"
        parse P.parseSExpr "" input `shouldParse` (L.SEMacroDef 
          (L.Macro 
            (mkPos 1, mkPos 2) 
            (mkPos 1, mkPos 30) 
            "boom" 
            (L.SEPrint 
              (mkPos 1, mkPos 14) 
              (mkPos 1, mkPos 29) 
              (L.SELiteral L.String' "explode" (mkPos 1, mkPos 20) (mkPos 1, mkPos 29)))))
      it "parse a macro call" $ do
        let input = "(call-macro mymac)"
        parse P.parseSExpr "" input `shouldParse` (L.SEMacroCall
          (mkPos 1, mkPos 2)
          (mkPos 1, mkPos 18)
          "mymac")
