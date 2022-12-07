module Main where

import qualified Task1Lib (task1Func)
import qualified Task2Lib (task2Func)

main :: IO ()
main = do
  fileLines <- fmap lines (readFile "app/data.txt")
  putStrLn "============= TASK 1 ============="
  Task1Lib.task1Func fileLines
  putStrLn "============= TASK 2 ============="
  Task2Lib.task2Func fileLines
