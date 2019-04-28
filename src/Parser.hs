module Parser where

import Data.Char
import Control.Monad
import Control.Applicative

-- A Parser is a function that takes as input a stream of characters and yields a parse tree, which is an pair
-- of a token and the string of characters that the token represents.
newtype Parser a = Parser { parse :: String -> [(a,String)] }


instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a,b) <- cs s])


instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])


instance Monad Parser where
  return = unit
  (>>=)  = bind


instance MonadPlus Parser where
  mzero = failure
  mplus = combine


instance Alternative Parser where
  empty = mzero
  (<|>) = option


option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res


unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])


bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s


failure :: Parser a
failure = Parser (\cs -> [])


combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)


runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)] -> error "Parser did not consume entire stream."
    _         -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else (Parser (\cs -> []))


oneOf :: [Char] -> Parser Char
oneOf s = satisfy ( flip elem s )


-- parse a literal character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- So the idea would be to load lines from a file
-- filter down to just ones that match a given regex?
-- Or do we want to filter down using pattern matching on the pattern that we're interested in?
-- How would we use pattern matching in this case? I think we have to pattern-match on literals, right?
-- So I guess we'd have to create a tokenized form of each line and match against it?
-- So we'd convert the line into a sequence of tokens, where the token is something like


-- some :: f a -> f [a]
-- some v = some_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v
--
-- many :: f a -> f [a]
-- many v = many_v
--   where
--     many_v = some_v <|> pure []
--     some_v = (:) <$> v <*> many_v




-- parse a natural number
natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

-- parse a string literal
string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

-- parse zero-or-more whitespace
spaces :: Parser String
spaces = many $ oneOf " \n\r"

-- parse literally anything
anything :: Parser String
anything = many item

-- parse a single digit
digit :: Parser Char
digit = satisfy isDigit

-- parse a positive or negative integer
number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

-- parse a single token from another parser and zero or more spaces after it and return the token
token :: Parser a -> Parser a
token p = do { a <- p; spaces ; return a }

-- parse a reserved word followed by zero or more spaces after it
reserved :: String -> Parser String
reserved s = token (string s)

-- parse a given parser wrapped in correctly-matched parentheses
parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

task :: Parser Bool
task = do
  -- optionally consume whitespace at the beginning of the line
  spaces
  string "- [ ] "
  anything
  return True

run :: String -> Bool
run = runParser task

