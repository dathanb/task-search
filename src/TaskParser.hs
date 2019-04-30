module TaskParser where

import Parser
import TaskLexer

data Date = Date Integer Integer Integer -- year month day
  deriving (Eq, Show)

data Line
  = Task String
  | DatedTask Date String
  | CompletedTask String
  deriving (Eq, Show)

unwrap (NumberToken a) = a

-- TODO: limit this to just YYYY-MM-DD format
date :: Parser Date
date = do
  y <- natural
  dash
  m <- natural
  dash
  d <- natural
  return $ Date (unwrap y) (unwrap m) (unwrap d)

taskWithoutDate :: Parser Line
taskWithoutDate = do
  whitespace
  dash
  space
  openBracket
  space
  closeBracket
  space
  Task <$> anything

taskWithDate :: Parser Line
taskWithDate = do
  whitespace
  dash
  space
  openBracket
  space
  closeBracket
  space
  greaterThanSign
  d <- date
  colon
  whitespace
  (DatedTask d) <$> anything

completedTask :: Parser Line
completedTask = do
  whitespace
  dash
  space
  openBracket
  char 'X'
  closeBracket
  space
  CompletedTask <$> anything

