module TaskLexer where

import Data.Char
import Parser
import Control.Applicative

data LexicalToken
  = DashToken
  | SpaceToken
  | OpenBracketToken
  | CloseBracketToken
  | GreaterThanToken
  | NumberToken Integer
  | WhitespaceToken
  | ColonToken
  deriving (Show, Eq)

dash :: Parser LexicalToken
dash = do
  c <- char '-'
  return DashToken

space :: Parser LexicalToken
space = do
  c <- char ' '
  return SpaceToken

openBracketToken :: Parser LexicalToken
openBracketToken = do
  c <- char '['
  return OpenBracketToken

closeBracketToken :: Parser LexicalToken
closeBracketToken = do
  c <- char '['
  return CloseBracketToken

greaterThanSign :: Parser LexicalToken
greaterThanSign = do
  c <- char '>'
  return GreaterThanToken

-- parse a single digit
digit :: Parser Char
digit = satisfy isDigit

-- parse a natural number
natural :: Parser LexicalToken
natural = NumberToken <$> (read <$> some (satisfy isDigit))

-- consume zero or more contiguous whitespace characters
whitespace :: Parser LexicalToken
whitespace = do
  s <- spaces
  return WhitespaceToken

-- literal ':'
colon :: Parser LexicalToken
colon = do
  s <- char ':'
  return ColonToken
