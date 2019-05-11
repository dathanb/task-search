module TaskParserSpec where

import Data.Either

import Test.Hspec
import Test.QuickCheck

import Text.Parsec
import Text.Parsec.Pos

import TaskParser

import TestUtils

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "taskWithoutDate" $ it "parses a task with no leading whitespace" $ 1 `shouldBe` 1
--      parse taskWithoutDate "- [ ] foo" `shouldBe` [(Task "foo", "")]
--    it "parses a task with leading whitespace" $
--      parse taskWithoutDate "    - [ ] foo" `shouldBe` [(Task "foo", "")]
--
--  describe "date" $ do
--    it "parses a yyyy-mm-dd date" $
--      parse date "2019-01-01" `shouldBe` [(Date 2019 1 1, "")]
--
--  describe "taskWithDate" $ do
--    it "parses a dated task with no leading whitespace" $
--      parse taskWithDate "- [ ] >2019-01-01: foo" `shouldBe` [(DatedTask (Date 2019 1 1) "foo", "")]
--    it "parses a dated task with leading whitespace" $
--      parse taskWithDate "    - [ ] >2019-01-01: foo" `shouldBe` [(DatedTask (Date 2019 1 1) "foo", "")]
--
  describe "completedTask" $
    it "parses a complete task" $ do
      let either = parse completedTask "N/A" "    - [X] anything"
      either `shouldSatisfy` isRight
      let task = getRight either
      getLineText task `shouldBe` "anything"
      getLinePosition task `shouldBe` newPos "N/A" 1 5

-- TODO: add position to lines, so we can find tasks and return their positions
