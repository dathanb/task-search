{-# LANGUAGE ViewPatterns #-}

module TaskLexerSpec where

import Data.Typeable
import Data.Either

import Text.Parsec hiding (space)
import Text.Parsec.Error
import Text.Parsec.Pos

import Test.Hspec
import Test.QuickCheck

import TaskLexer

main :: IO()
--main = hspec spec
main = do
  let res = parse dash "N/A" "pther"
  let err = fromLeft (newErrorUnknown (newPos "N/A" 1 1)) res
  printMsgs $ errorMessages err


printMsgs :: [Message] -> IO()
printMsgs [] = return ()
printMsgs (m:ms) = do
  putStrLn (messageString m)
  printMsgs ms


hasErrorMessage :: String -> Either ParseError a -> Bool
hasErrorMessage expected (Left (errorMessages -> msgs)) = elem expected $ fmap messageString msgs


spec :: Spec
spec = do
  describe "dash" $ do
    it "parses a dash" $
      parse dash "N/A" "-" `shouldBe` Right DashToken
    it "fails on non-dash" $
      parse dash "N/A" "other" `shouldSatisfy` isLeft

  describe "space" $ do
    it "parses a single space" $
      parse space "N/A" " " `shouldBe` Right SpaceToken
    it "fails on non-space" $
      parse space "N/A" "foo" `shouldSatisfy` isLeft
    it "consumes only a single space when there are multiple" $
      pending
--      parse space "N/A" "  " `shouldBe` (Right ' ')

  describe "openBracket" $ do
    it "parses a single open bracket" $
      parse openBracket "N/A" "[" `shouldBe` Right OpenBracketToken
    it "fails on non-open-bracket" $
      parse openBracket "N/A" " " `shouldSatisfy` isLeft
    it "consumes only a single open bracket when there are multiple" $
      pending
--      parse openBracket "N/A" "[[" `shouldBe` (Right OpenBracketToken)

  describe "closeBracket" $ do
    it "parses a single close bracket" $
      parse closeBracket "N/A" "]" `shouldBe` Right CloseBracketToken
    it "fails on non-close-bracket" $
      parse closeBracket "N/A" " " `shouldSatisfy` isLeft
    it "consumes only a single close bracket when there are multiple" $
      pending
--      parse closeBracket "N/A" "]]" `shouldBe` [(CloseBracketToken, "]")]

  describe "greaterThanSign" $ do
    it "parses a single greater than sign" $
      parse greaterThanSign "N/A" ">" `shouldBe` Right GreaterThanToken
    it "fails on non-greater-than-sign" $
      parse greaterThanSign "N/A" "foo" `shouldSatisfy` isLeft
    it "consumes only a single close bracker when there are multiple" $
      pending
--      parse greaterThanSign ">>" `shouldBe` [(GreaterThanToken, ">")]

  describe "digit" $ do
    it "parses a single digit" $ do
      parse digit "N/A" "0" `shouldBe` Right '0'
      parse digit "N/A" "1" `shouldBe` Right '1'
      parse digit "N/A" "2" `shouldBe` Right '2'
      parse digit "N/A" "3" `shouldBe` Right '3'
      parse digit "N/A" "4" `shouldBe` Right '4'
      parse digit "N/A" "5" `shouldBe` Right '5'
      parse digit "N/A" "6" `shouldBe` Right '6'
      parse digit "N/A" "7" `shouldBe` Right '7'
      parse digit "N/A" "8" `shouldBe` Right '8'
      parse digit "N/A" "9" `shouldBe` Right '9'
    it "fails on non-digit" $
      parse digit "N/A" "foo" `shouldSatisfy` isLeft
    it "consumes only a single digit when there are multiple" $
      pending
--      parse digit "12" `shouldBe` [('1', "2")]

  describe "natural" $ do
    it "parses a one-digit number" $
      parse natural "N/A" "0" `shouldBe` Right (NumberToken 0)
    it "parses a two-digit number" $
      parse natural "N/A" "21" `shouldBe` Right (NumberToken 21)
    it "consumes up to first non-digit" $
      pending
--      parse natural "N/A" "123f" `shouldBe` Right (NumberToken 123)
    it "fails on non-number" $
      parse natural "N/A" "foo" `shouldSatisfy` isLeft

  describe "whitespace" $ do
    it "parses a single whitespace" $
      parse whitespace "N/A" " " `shouldBe` Right WhitespaceToken
    it "parses multiple whitespace" $
      parse whitespace "N/A" "    " `shouldBe` Right WhitespaceToken
    it "parses up to the first non-whitespace" $
      pending
      --parse whitespace "N/A" "    foo" `shouldBe` Right WhitespaceToken
    it "parses empty whitespace" $
      parse whitespace "N/A" "foo" `shouldBe` Right WhitespaceToken

  describe "colon" $ do
    it "parses a single colon" $
      parse colon "N/A" ":" `shouldBe` Right ColonToken
    it "fails on non-colon" $
      parse colon "N/A" "foo" `shouldSatisfy` isLeft
    it "consumes only a single colon when there are multiple" $
      pending
--      parse colon "N/A" "::" `shouldBe` [(ColonToken, ":")]

