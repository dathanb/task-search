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
      parse dash "-" `shouldBe` [('-', "")]
    it "fails on non-dash" $ do
      parse dash "other" `shouldBe` []

