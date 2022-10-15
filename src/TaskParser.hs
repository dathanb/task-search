module TaskParser where

import TaskLexer

import Text.Parsec hiding (space, Line)
import Text.ParserCombinators.Parsec (GenParser, ParseError, many)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Date = Date Integer Integer Integer -- year month day
  deriving (Eq, Show)

data Line
  = Task SourcePos String
  | DatedTask SourcePos Date String
  | CompletedTask SourcePos String
  | NonTask SourcePos String
  deriving (Eq, Show)

data LineStatus
  = Incomplete
  | Complete
  | Future
  | NotATask

unwrap (NumberToken a) = a

--parseFile :: String -> Either ParseError [[Line]]
--parseFile input = parse markdownFile "(unknown)" input

--markdownFile :: GenParser Char st [Line]
--markdownFile = do
--  result <- many line
--  eof
--  return result


line :: GenParser Char str Line
line = do
  result <- do
    taskWithDate
    <|> taskWithoutDate
    <|> completedTask
    <|> nonTask
  eol
  return result


nonTask :: GenParser Char st Line
nonTask = do
  pos <- getPosition
  NonTask pos <$> restOfLine

-- TODO: limit this to just YYYY-MM-DD format
date :: GenParser Char st Date
date = do
  y <- natural
  dash
  m <- natural
  dash
  d <- natural
  return $ Date (unwrap y) (unwrap m) (unwrap d)

taskWithoutDate :: GenParser Char st Line
taskWithoutDate = do
  whitespace
  pos <- getPosition
  dash
  space
  openBracket
  space
  closeBracket
  space
  Task pos <$> restOfLine

taskWithDate :: GenParser Char st Line
taskWithDate = do
  whitespace
  pos <- getPosition
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
  DatedTask pos d <$> restOfLine

completedTask :: GenParser Char st Line
completedTask = do
  whitespace
  pos <- getPosition
  dash
  space
  openBracket
  char 'X'
  closeBracket
  space
  CompletedTask pos <$> restOfLine


getLineText :: Line -> String
getLineText (Task _ s) = s
getLineText (DatedTask _ _ s) = s
getLineText (CompletedTask _ s) = s
getLineText (NonTask _ s) = s

getLinePosition :: Line -> SourcePos
getLinePosition (Task p _) = p
getLinePosition (DatedTask p _ _) = p
getLinePosition (CompletedTask p _) = p
getLinePosition (NonTask p _) = p

getLineDate :: Line -> Maybe Date
getLineDate (Task _ _) = Nothing
getLineDate (CompletedTask _ _) = Nothing
getLineDate (DatedTask _ d _) = Just d
getLineDate (NonTask _ _) = Nothing

getTaskStatus :: Line -> LineStatus
getTaskStatus (Task _ _) = Incomplete
getTaskStatus (DatedTask _ d _) = Incomplete
getTaskStatus (CompletedTask _ _) = Complete
getTaskStatus (NonTask _ _) = NotATask
