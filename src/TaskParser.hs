module TaskParser where

import TaskLexer

import Text.ParserCombinators.Parsec (GenParser, ParseError, many)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Date = Date Integer Integer Integer -- year month day
  deriving (Eq, Show)

data Line
  = Task String
  | DatedTask Date String
  | CompletedTask String
  deriving (Eq, Show)

unwrap (NumberToken a) = a

--markdownFile :: GenParser Char st [[Line]]
--markdownFile = do
--  result <- many line
--  eof
--  return result
--
--
--line :: GenParser Char str String
--line = do
--  result <- do
--    taskWithDate
--    <|> taskWithoutDate
--    <|> completedTask
--    <|> nonTask
--  eol
--  return result
--
--
---- TODO: limit this to just YYYY-MM-DD format
--date :: GenParser Char st Date
--date = do
--  y <- natural
--  dash
--  m <- natural
--  dash
--  d <- natural
--  return $ Date (unwrap y) (unwrap m) (unwrap d)
--
--taskWithoutDate :: GenParser Char st Line
--taskWithoutDate = do
--  whitespace
--  dash
--  space
--  openBracket
--  space
--  closeBracket
--  space
--  Task <$> anything
--
--taskWithDate :: GenParser Char st Line
--taskWithDate = do
--  whitespace
--  dash
--  space
--  openBracket
--  space
--  closeBracket
--  space
--  greaterThanSign
--  d <- date
--  colon
--  whitespace
--  (DatedTask d) <$> anything
--
--completedTask :: GenParser Char st Line
--completedTask = do
--  whitespace
--  dash
--  space
--  openBracket
--  char 'X'
--  closeBracket
--  space
--  CompletedTask <$> anything
--
--parseFile :: String -> Either ParseError [[Line]]
--parseFile input = parse markdownFile "(unknown)" input
