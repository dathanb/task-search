module Main where

import Data.String.Strip
--import Text.ParserCombinators.Parsec
import Parser

parseTask :: String -> Bool
parseTask = runParser task

main :: IO()
main = do
  line <- getLine
  putStrLn $ show $ parseTask line

--main :: IO ()
--main = interact $ \line ->
--  when ( ( not . null ) line ) $ do
--    putStrLn ( show ( run line ) )
--    main
--main = forever $ do
--  putStr "> "
--  a <- getLine
--  print $ eval $ run a

