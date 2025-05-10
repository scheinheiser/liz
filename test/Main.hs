{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Megaparsec

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Liz.Parser as P

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

  -- SEVar T.Text SExpr SExpr -- ident - type - value
  describe "Variable declaration parsing" $ do
    it "parse a variable with explicit typing" $ do
      parse P.parseVarDecl "" "var hello String \"World\"" `shouldParse` (P.SEVar "hello" (P.SEType P.String') (P.SELiteral "\"World\""))

    it "parse a variable and infer its type" $ do
      parse P.parseVarDecl "" "var four 4" `shouldParse` (P.SEVar "four" (P.SEType P.Int') (P.SELiteral "4"))

    it "parse a constant with explicit typing" $ do
      parse P.parseVarDecl "" "const tau Float 6.283185" `shouldParse` (P.SEConst "tau" (P.SEType P.Float') (P.SELiteral "6.283185"))

    it "parse a constant and infer its type" $ do
      parse P.parseVarDecl "" "const pi 3.141592" `shouldParse` (P.SEConst "pi" (P.SEType P.Float') (P.SELiteral "3.141592"))

    it "parse a boolean value" $ do
      parse P.parseVarDecl "" "var mybool Bool True" `shouldParse` (P.SEVar "mybool" (P.SEType P.Bool') (P.SELiteral "True"))
