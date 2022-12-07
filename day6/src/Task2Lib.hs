module Task2Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt)
import Data.Containers.ListUtils (nubOrd)
import Data.List (nub)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "InputLine:"
    let inputLine = head inputLines
    print inputLine
    putStrLn "Marker:"
    let marker = calcMarker inputLine 14
    print marker

calcMarker :: String -> Int -> Int
calcMarker (c1:cs) pos
    | uniqueLength == 14 = pos
    | otherwise = calcMarker cs pos + 1
    where uniqueLength = length . nub $ take 14 (c1:cs)