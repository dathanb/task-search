module TaskLexer where

import Data.Char
import Parser

data Token =
  Dash

dash :: Parser Char
dash = char '-'

