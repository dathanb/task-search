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
  describe "taskWithoutDate" $ do
    it "parses a task with no leading whitespace" $ do
      let res = parse taskWithoutDate "" "- [ ] foo"
      res `shouldSatisfy` isRight
      let right = getRight res
      getLineText right `shouldBe` "foo"
      getLinePosition right `shouldBe` newPos "" 1 1

    it "parses a task with leading whitespace" $ do
      let res = parse taskWithoutDate "" "    - [ ] foo"
      res `shouldSatisfy` isRight
      let right = getRight res
      getLineText right `shouldBe` "foo"
      getLinePosition right `shouldBe` newPos "" 1 5

    it "consumes only until the end of the line" $ do
      let res = parse taskWithoutDate "" "    - [ ] foo\nnext"
      res `shouldSatisfy` isRight
      let right = getRight res
      getLineText right `shouldBe` "foo"
      getLinePosition right `shouldBe` newPos "" 1 5

    it "is still a task if it has no label" $ do
      let res = parse taskWithoutDate "" "    - [ ] \nnext"
      res `shouldSatisfy` isRight
      let right = getRight res
      getLineText right `shouldBe` ""
      getLinePosition right `shouldBe` newPos "" 1 5

    it "fails to parse a line without a task" $ do
      let res = parse taskWithoutDate "" "something that's not a task"
      res `shouldSatisfy` isLeft

  describe "date" $ do
    it "parses a yyyy-mm-dd date" $ do
      let res = parse date "" "2019-01-01"
      res `shouldSatisfy` isRight
      let right = getRight res
      right `shouldBe` Date 2019 1 1
    it "fails on a non-date" $ do
      let res = parse date "" "foo"
      res `shouldSatisfy` isLeft

  describe "taskWithDate" $ do
    it "parses a dated task with no leading whitespace" $ do
      let res = parse taskWithDate "" "- [ ] >2019-01-01: foo"
      res `shouldSatisfy` isRight
      let right = getRight res
      getLinePosition right `shouldBe` newPos "" 1 1
      getLineText right `shouldBe` "foo"
      getLineDate right `shouldBe` Just (Date 2019 1 1)
    it "parses a dated task with leading whitespace" $ do
      let res = parse taskWithDate "" "    - [ ] >2019-01-01: foo"
      res `shouldSatisfy` isRight
      let right = getRight res
      getLineText right `shouldBe` "foo"
      getLinePosition right `shouldBe` newPos "" 1 5
      getLineDate right `shouldBe` Just (Date 2019 1 1)
    it "fails to parse a non-dated task" $ do
      let res = parse taskWithDate "" "    - [ ] foo"
      res `shouldSatisfy` isLeft

  describe "completedTask" $
    it "parses a complete task" $ do
      let either = parse completedTask "N/A" "    - [X] anything"
      either `shouldSatisfy` isRight
      let task = getRight either
      getLineText task `shouldBe` "anything"
      getLinePosition task `shouldBe` newPos "N/A" 1 5

  describe "nonTask" $ do
    it "consumes a line" $ do
      let either = parse nonTask "N/A" "foo"
      either `shouldSatisfy` isRight
      let line = getRight either
      getLineText line `shouldBe` "foo"
      getLinePosition line `shouldBe` newPos "N/A" 1 1
    it "doesn't conusume the next line" $ do
      let either = parse nonTask "" "foo\nbar"
      either `shouldSatisfy` isRight
      let line = getRight either
      getLineText line `shouldBe` "foo"
      getLinePosition line `shouldBe` newPos "" 1 1

  describe "line" $ do
    it "consumes a task with a date" $ do
      let either = parse nonTask "" "- [ ] >2019-05-01: foo"
      either `shouldSatisfy` isRight
      let line = getRight either
      getLineText line `shouldBe` "foo"
      getLinePosition line `shouldBe` newPos "" 1 1
      getLineDate line `shouldBe` Just (Date 2019 1 1)
    it "consumes a task without a date" $ do
      let either = parse nonTask "" "- [ ] foo"
      either `shouldSatisfy` isRight
      let line = getRight either
      getLineText line `shouldBe` "foo"
      getLinePosition line `shouldBe` newPos "" 1 1
      getLineDate line `shouldBe` Nothing
    it "consumes a completed task" $ do
      let either = parse nonTask "" "- [ ] foo"
      either `shouldSatisfy` isRight
      let line = getRight either
      getLineText line `shouldBe` "foo"
      getLinePosition line `shouldBe` newPos "" 1 1
      getLineDate line `shouldBe` Nothing


-- TODO: add position to lines, so we can find tasks and return their positions
