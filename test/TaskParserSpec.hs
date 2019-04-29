module TaskParserSpec where

import Test.Hspec
import Test.QuickCheck

import TaskParser
import Parser

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "taskWithoutDate" $ do
    it "parses a task with no leading whitespace" $
      parse taskWithoutDate "- [ ] foo" `shouldBe` [(Task "foo", "")]
    it "parses a task with leading whitespace" $
      parse taskWithoutDate "    - [ ] foo" `shouldBe` [(Task "foo", "")]

  describe "date" $ do
    it "parses a yyyy-mm-dd date" $
      parse date "2019-01-01" `shouldBe` [(Date 2019 1 1, "")]

  describe "taskWithDate" $ do
    it "parses a dated task with no leading whitespace" $
      parse taskWithDate "- [ ] >2019-01-01: foo" `shouldBe` [(DatedTask (Date 2019 1 1) "foo", "")]
    it "parses a dated task with leading whitespace" $
      parse taskWithDate "    - [ ] >2019-01-01: foo" `shouldBe` [(DatedTask (Date 2019 1 1) "foo", "")]

