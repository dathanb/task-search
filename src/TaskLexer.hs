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

-- parse a positive or negative integer
number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

-- parse a natural number
natural :: Parser LexicalToken
natural = NumberToken <$> (read <$> some (satisfy isDigit))

whitespace :: Parser LexicalToken
whitespace = do
  s <- spaces
  return WhitespaceToken
