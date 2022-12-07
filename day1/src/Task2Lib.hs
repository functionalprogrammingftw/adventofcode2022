module Task2Lib (task2Func) where

import qualified UtilLib (readInt, maximum3, sumTupleElements)

task2Func :: [String] -> IO ()
task2Func caloriesLines = do
    putStrLn "Elf sums: "
    print $ sumElfCaloriesLists $ createElfCaloriesLists [] caloriesLines
    putStrLn "Max 3 number of calories carried by elf: "
    print $ UtilLib.maximum3 . sumElfCaloriesLists $ createElfCaloriesLists [] caloriesLines
    putStrLn "Sum of calories carried 3 elves carrying the most: "
    print $ UtilLib.sumTupleElements . UtilLib.maximum3 . sumElfCaloriesLists $ createElfCaloriesLists [] caloriesLines

sumElfCaloriesLists :: [[Int]] -> [Int]
sumElfCaloriesLists = map sum

createElfCaloriesLists :: [[Int]] -> [String] -> [[Int]]
createElfCaloriesLists acc [] = acc
createElfCaloriesLists [] (str:strs) = createElfCaloriesLists [[UtilLib.readInt str]] strs
createElfCaloriesLists acc (str:strs) =
    if str == ""
        then createElfCaloriesLists (acc ++ [[]]) strs
        else createElfCaloriesLists (init acc ++ [last acc ++ [UtilLib.readInt str]]) strs
