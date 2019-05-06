module Main where

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos
import Data.Either
import TaskLexer

main :: IO()
main = do
--  let pr = runParser dash "" "" "other"
--  let pe = fromLeft (newErrorUnknown (newPos "" 1 1)) pr
  parseTest dash "other"
--  contents <- getContents
--  putStrLn $ show $ parseTask contents
--  putStrLn contents

--main :: IO ()
--main = interact $ \line ->
--  when ( ( not . null ) line ) $ do
--    putStrLn ( show ( run line ) )
--    main
--main = forever $ do
--  putStr "> "
--  a <- getLine
--  print $ eval $ run a

