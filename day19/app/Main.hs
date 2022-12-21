module Main where

import qualified Task1Lib (taskFunc)
import qualified Task2Lib (taskFunc)

main :: IO ()
main = do
  fileLines <- fmap lines (readFile "app/smalldata.txt")
  putStrLn "============= TASK 1 ============="
  Task1Lib.taskFunc fileLines
  putStrLn "============= TASK 2 ============="
  Task2Lib.taskFunc fileLines
