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
    it "parses a dash" $
      parse dash "-" `shouldBe` [(DashToken, "")]
    it "fails on non-dash" $
      parse dash "other" `shouldBe` []

  describe "space" $ do
    it "parses a single space" $
      parse space " " `shouldBe` [(SpaceToken, "")]
    it "fails on non-space" $
      parse space "foo" `shouldBe` []
    it "consumes only a single space when there are multiple" $
      parse space "  " `shouldBe` [(SpaceToken, " ")]

  describe "openBracket" $ do
    it "parses a single open bracket" $
      parse openBracket "[" `shouldBe` [(OpenBracketToken, "")]
    it "fails on non-open-bracket" $
      parse openBracket " " `shouldBe` []
    it "consumes only a single open bracket when there are multiple" $
      parse openBracket "[[" `shouldBe` [(OpenBracketToken, "[")]

  describe "closeBracket" $ do
    it "parses a single close bracket" $
      parse closeBracket "]" `shouldBe` [(CloseBracketToken, "")]
    it "fails on non-close-bracket" $
      parse closeBracket " " `shouldBe` []
    it "consumes only a single close bracket when there are multiple" $
      parse closeBracket "]]" `shouldBe` [(CloseBracketToken, "]")]

  describe "greaterThanSign" $ do
    it "parses a single greater than sign" $
      parse greaterThanSign ">" `shouldBe` [(GreaterThanToken, "")]
    it "fails on non-greater-than-sign" $
      parse greaterThanSign "foo" `shouldBe` []
    it "consumes only a single close bracker when there are multiple" $
      parse greaterThanSign ">>" `shouldBe` [(GreaterThanToken, ">")]

  describe "digit" $ do
    it "parses a single digit" $ do
      parse digit "0" `shouldBe` [('0', "")]
      parse digit "1" `shouldBe` [('1', "")]
      parse digit "2" `shouldBe` [('2', "")]
      parse digit "3" `shouldBe` [('3', "")]
      parse digit "4" `shouldBe` [('4', "")]
      parse digit "5" `shouldBe` [('5', "")]
      parse digit "6" `shouldBe` [('6', "")]
      parse digit "7" `shouldBe` [('7', "")]
      parse digit "8" `shouldBe` [('8', "")]
      parse digit "9" `shouldBe` [('9', "")]
    it "fails on non-digit" $
      parse digit "foo" `shouldBe` []
    it "consumes only a single digit when there are multiple" $
      parse digit "12" `shouldBe` [('1', "2")]

  describe "natural" $ do
    it "parses a one-digit number" $
      parse natural "0" `shouldBe` [(NumberToken 0, "")]
    it "parses a two-digit number" $
      parse natural "21" `shouldBe` [(NumberToken 21, "")]
    it "consumes up to first non-digit" $
      parse natural "123f" `shouldBe` [(NumberToken 123, "f")]
    it "fails on non-number" $
      parse natural "foo" `shouldBe` []

  describe "whitespace" $ do
    it "parses a single whitespace" $
      parse whitespace " " `shouldBe` [(WhitespaceToken, "")]
    it "parses multiple whitespace" $
      parse whitespace "    " `shouldBe` [(WhitespaceToken, "")]
    it "parses up to the first non-whitespace" $
      parse whitespace "    foo" `shouldBe` [(WhitespaceToken, "foo")]
    it "parses empty whitespace" $
      parse whitespace "foo" `shouldBe` [(WhitespaceToken, "foo")]

  describe "colon" $ do
    it "parses a single colon" $
      parse colon ":" `shouldBe` [(ColonToken, "")]
    it "fails on non-colon" $
      parse colon "foo" `shouldBe` []
    it "consumes only a single colon when there are multiple" $
      parse colon "::" `shouldBe` [(ColonToken, ":")]

