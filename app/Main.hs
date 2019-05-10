module Main where

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import Data.Either
import TaskLexer


main :: IO()
main = do
  let pr = runParser dash "" "" "- other"
  case pr of
    Left error -> putStrLn "Encountered an error"
    Right dash -> print dash

-- TODO: write a parser that will wrap existing parsers and return the position of a task match if and only if the task parser matches

