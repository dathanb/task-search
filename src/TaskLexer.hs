module TaskLexer where

import Data.Char
import Parser
import Control.Applicative

data Token
  = DashToken
  | SpaceToken
  | OpenBracketToken
  | CloseBracketToken
  | GreaterThanToken
  | NumberToken Integer
  | WhitespaceToken

dash :: Parser Token
dash = do
  c <- char '-'
  return DashToken

space :: Parser Token
space = do
  c <- char ' '
  return SpaceToken

openBracketToken :: Parser Token
openBracketToken = do
  c <- char '['
  return OpenBracketToken

closeBracketToken :: Parser Token
closeBracketToken = do
  c <- char '['
  return CloseBracketToken

greaterThanSign :: Parser Token
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
natural :: Parser Token
natural = NumberToken <$> (read <$> some (satisfy isDigit))

whitespace :: Parser Token
whitespace = do
  s <- spaces
  return WhitespaceToken
