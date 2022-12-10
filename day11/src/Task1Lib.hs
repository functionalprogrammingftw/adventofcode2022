module Task1Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt, countTrueGrid)
import Data.List (nub, stripPrefix, insert, intercalate)
import qualified Data.Map (Map, empty, lookup, insert, foldr)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Input lines:"
    print inputLines



parseInputLines :: [String] -> [Int]
parseInputLines [] = []
parseInputLines (line:lines) = count:parseInputLines lines
    where splitted = splitOn " " line
          count
            | head splitted == "noop" = 0
            | otherwise = readInt $ last splitted

findSignalStrengths :: [Int] -> [Int] -> [Int]
findSignalStrengths [] sumCounts = []
findSignalStrengths (x:xs) sumCounts = (x * (sumCounts !! (x - 1))):findSignalStrengths xs sumCounts


calcSumCounts :: [Int] -> [Int]
calcSumCounts = reverse . subCalcSumCounts . reverse

subCalcSumCounts :: [Int] -> [Int]
subCalcSumCounts [] = [1]
subCalcSumCounts (0:cs) = prevSum:prevSums
    where prevSum = head prevSums
          prevSums = subCalcSumCounts cs
subCalcSumCounts (c:cs) = newSum:prevSum:prevSums
    where newSum = c + prevSum
          prevSum = head prevSums
          prevSums = subCalcSumCounts cs
