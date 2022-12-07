module Task2Lib (task2Func) where

import qualified UtilLib (readInt, tuplify2)
import Data.Char (ord, isLower)
import Data.List.Split (splitOn)
import UtilLib (tuplify2, elems, elemsOverlap)

task2Func :: [String] -> IO ()
task2Func inputLines = do
    putStrLn "Input data:"
    let tuples = splitListIntoTuples inputLines
    print tuples
    putStrLn "Lists:"
    let lists = map createLists tuples
    print lists
    putStrLn "One list contains other:"
    let listOfOverlapsOther = map oneListOverlapsOther lists
    print listOfOverlapsOther
    putStrLn "Count:"
    print $ sum listOfOverlapsOther

splitListIntoTuples :: [String] -> [((Int, Int), (Int, Int))]
splitListIntoTuples = map splitIntoTuples

splitIntoTuples :: String -> ((Int, Int), (Int, Int))
splitIntoTuples str = tuplify2 $ map splitIntoTuple $ splitOn "," str

splitIntoTuple :: String -> (Int, Int)
splitIntoTuple str = UtilLib.tuplify2 $ map UtilLib.readInt $ splitOn "-" str

createLists :: ((Int, Int), (Int, Int)) -> ([Int], [Int])
createLists ((x1, x2), (y1, y2)) = ([x1..x2], [y1..y2])

oneListOverlapsOther :: ([Int], [Int]) -> Int
oneListOverlapsOther (xs, ys)
    | UtilLib.elemsOverlap xs ys = 1
    | otherwise = 0
