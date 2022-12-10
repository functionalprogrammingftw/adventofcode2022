module Task2Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt, countTrueGrid, splitEvery)
import Data.List (nub, stripPrefix, insert, intercalate)
import qualified Data.Map (Map, empty, lookup, insert, foldr)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Counts:"
    let counts = parseInputLines inputLines
    print counts
    putStrLn "Register X values:"
    let registerXValues = calcRegisterXValues counts
    print registerXValues
    putStrLn "CRT pos and pixel poses:"
    let crtPosAndPixelPoses = calcCrtPosAndPixelPoses $ calcPixelPoses registerXValues
    print crtPosAndPixelPoses
    putStrLn "Pixels:"
    let pixels = calcPixels crtPosAndPixelPoses
    printLines $ splitEvery 40 pixels

parseInputLines :: [String] -> [Int]
parseInputLines [] = []
parseInputLines (line:lines) = count:parseInputLines lines
    where splitted = splitOn " " line
          count
            | head splitted == "noop" = 0
            | otherwise = readInt $ last splitted

printLines :: [String] -> IO ()
printLines [] = do
    return ()
printLines (str:strs) = do
    print str
    printLines strs

calcPixels :: [(Int, [Int])] -> String
calcPixels [] = []
calcPixels ((crtPos, pixelPoses):crtPosAndPixelPoses) = pixel:calcPixels crtPosAndPixelPoses
    where pixel
            | crtPos `elem` pixelPoses = 'X'
            | otherwise = '.'

calcCrtPosAndPixelPoses :: [[Int]] -> [(Int, [Int])]
calcCrtPosAndPixelPoses = zip [ x `mod` 40 | x <- [0..] ]

calcPixelPoses :: [Int] -> [[Int]]
calcPixelPoses = map (\x -> [x - 1, x, x + 1])

calcRegisterXValues :: [Int] -> [Int]
calcRegisterXValues = reverse . subCalcSumCounts . reverse

subCalcSumCounts :: [Int] -> [Int]
subCalcSumCounts [] = [1]
subCalcSumCounts (0:cs) = prevSum:prevSums
    where prevSum = head prevSums
          prevSums = subCalcSumCounts cs
subCalcSumCounts (c:cs) = newSum:prevSum:prevSums
    where newSum = c + prevSum
          prevSum = head prevSums
          prevSums = subCalcSumCounts cs
