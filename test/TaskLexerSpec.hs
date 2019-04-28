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

