module TaskLexerSpec where

import Test.Hspec
import Test.QuickCheck

import Parser
import TaskLexer

main :: IO()
main = hspec spec

spec ::Spec
spec = do
  describe "dash" $ do
    it "parses a dash" $ do
      parse dash "-" `shouldBe` [(DashToken, "")]
    it "fails on non-dash" $ do
      parse dash "other" `shouldBe` []

  describe "space" $ do
    it "parses a single space" $ do
      parse space " " `shouldBe` [(SpaceToken, "")]
    it "fails on non-space" $ do
      parse space "foo" `shouldBe` []
    it "consumes only a single space when there are multiple" $ do
      parse space "  " `shouldBe` [(SpaceToken, " ")]

  describe "openBracket" $ do
    it "parses a single open bracket" $ do
      parse openBracket "[" `shouldBe` [(OpenBracketToken, "")]
    it "fails on non-open-bracket" $ do
      parse openBracket " " `shouldBe` []
    it "consumes only a single open bracket when there are multiple" $ do
      parse openBracket "[[" `shouldBe` [(OpenBracketToken, "[")]

  describe "closeBracket" $ do
    it "parses a single close bracket" $ do
      parse closeBracket "]" `shouldBe` [(CloseBracketToken, "")]
    it "fails on non-close-bracket" $ do
      parse closeBracket " " `shouldBe` []
    it "consumes only a single close bracket when there are multiple" $ do
      parse closeBracket "]]" `shouldBe` [(CloseBracketToken, "]")]

  describe "greaterThanSign" $ do
    it "parses a single greater than sign" $ do
      parse greaterThanSign ">" `shouldBe` [(GreaterThanToken, "")]
    it "fails on non-greater-than-sign" $ do
      parse greaterThanSign "foo" `shouldBe` []
    it "consumes only a single close bracker when there are multiple" $ do
      parse greaterThanSign ">>" `shouldBe` [(GreaterThanToken, ">")]

