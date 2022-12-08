module Task2Lib (task2Func) where

import qualified UtilLib (readInt, splitEvery)
import Data.Char (ord, isLower)

task2Func :: [String] -> IO ()
task2Func contentLines = do
    putStrLn "Split into 3 elf lists: "
    print $ UtilLib.splitEvery 3 contentLines
    putStrLn "Char in all 3 lists: "
    print $ map findCharInAllStrings $ UtilLib.splitEvery 3 contentLines
    putStrLn "Priority: "
    print $ map (calcPriority . findCharInAllStrings) $ UtilLib.splitEvery 3 contentLines
    putStrLn "Sum: "
    print $ sum $ map (calcPriority . findCharInAllStrings) $ UtilLib.splitEvery 3 contentLines

findCharInAllStrings :: [String] -> Char
findCharInAllStrings ((c:cs):strs)
    | isCharInAllStrings c strs = c
    | otherwise = findCharInAllStrings (cs:strs)

isCharInAllStrings :: Char -> [String] -> Bool
isCharInAllStrings c (str:strs)
    | c `notElem` str = False
    | otherwise = isCharInAllStrings c strs
isCharInAllStrings c [] = True

calcPriority :: Char -> Int
calcPriority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27
