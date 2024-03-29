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
  | WhitespaceToken
  | ColonToken
  deriving (Show, Eq)

data NumberToken = NumberToken Integer deriving (Eq, Show)


dash :: Parser LexicalToken
dash = do
  c <- char '-'
  return DashToken

space :: Parser LexicalToken
space = do
  c <- char ' '
  return SpaceToken

openBracket :: Parser LexicalToken
openBracket = do
  c <- char '['
  return OpenBracketToken

closeBracket :: Parser LexicalToken
closeBracket = do
  c <- char ']'
  return CloseBracketToken

greaterThanSign :: Parser LexicalToken
greaterThanSign = do
  c <- char '>'
  return GreaterThanToken

-- parse a single digit
digit :: Parser Char
digit = satisfy isDigit

-- parse a natural number
natural :: Parser NumberToken
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

--
