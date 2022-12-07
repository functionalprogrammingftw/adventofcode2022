module Task1Lib (task1Func) where

import qualified UtilLib (readInt)
import Data.Char (ord, isLower)

task1Func :: [String] -> IO ()
task1Func contentLines = do
    putStrLn "Compartment contents: "
    print $ map splitIntoCompartments contentLines
    putStrLn "Items in both compartments: "
    print $ map (itemInBothCompartments . splitIntoCompartments) contentLines
    putStrLn "Priorities of items in both compartments: "
    print $ map (calcPriority . itemInBothCompartments . splitIntoCompartments) contentLines
    putStrLn "Sum of priorities of items in both compartments: "
    print $ sum $ map (calcPriority . itemInBothCompartments . splitIntoCompartments) contentLines

splitIntoCompartments :: String -> (String, String)
splitIntoCompartments str = splitAt (length str `div` 2) str

itemInBothCompartments :: (String, String) -> Char
itemInBothCompartments (x:xs, ys)
    | x `elem` ys = x
    | otherwise = itemInBothCompartments (xs, ys)

calcPriority :: Char -> Int
calcPriority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27
