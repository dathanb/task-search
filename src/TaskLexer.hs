module TaskLexer where

import Data.Char

import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data LexicalToken
  = DashToken
  | SpaceToken
  | OpenBracketToken
  | CloseBracketToken
  | GreaterThanToken
  | WhitespaceToken
  | ColonToken
  deriving (Show, Eq)


newtype NumberToken = NumberToken Integer deriving (Eq, Show)

dash :: GenParser Char st LexicalToken
dash = do
  c <- char '-'
  return DashToken

space :: GenParser Char st LexicalToken
space = do
  c <- char ' '
  return SpaceToken

openBracket :: GenParser Char st LexicalToken
openBracket = do
  c <- char '['
  return OpenBracketToken

closeBracket :: GenParser Char st LexicalToken
closeBracket = do
  c <- char ']'
  return CloseBracketToken

greaterThanSign :: GenParser Char st LexicalToken
greaterThanSign = do
  c <- char '>'
  return GreaterThanToken

---- parse a natural number
natural :: GenParser Char st NumberToken
natural = NumberToken <$> (read <$> many1 (satisfy isDigit))

---- consume zero or more contiguous whitespace characters
whitespace :: GenParser Char st LexicalToken
whitespace = do
  s <- spaces
  return WhitespaceToken

---- literal ':'
colon :: GenParser Char st LexicalToken
colon = do
  s <- char ':'
  return ColonToken

anything :: GenParser Char st String
anything = many anyChar

restOfLine :: GenParser Char st String
restOfLine = many $ satisfy (/= '\n')

eol :: GenParser Char st Char
eol = satisfy (== '\n')
