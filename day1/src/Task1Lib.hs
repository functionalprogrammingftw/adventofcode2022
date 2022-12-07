module Task1Lib (task1Func) where

import qualified UtilLib (readInt)

task1Func :: [String] -> IO ()
task1Func caloriesLines = do
    putStrLn "Elf sums: "
    print $ sumElfCaloriesLists $ createElfCaloriesLists [] caloriesLines
    putStrLn "Max number of calories carried by elf: "
    print $ maximum . sumElfCaloriesLists $ createElfCaloriesLists [] caloriesLines

sumElfCaloriesLists :: [[Int]] -> [Int]
sumElfCaloriesLists = map sum

createElfCaloriesLists :: [[Int]] -> [String] -> [[Int]]
createElfCaloriesLists acc [] = acc
createElfCaloriesLists [] (str:strs) = createElfCaloriesLists [[UtilLib.readInt str]] strs
createElfCaloriesLists acc (str:strs) =
    if str == ""
        then createElfCaloriesLists (acc ++ [[]]) strs
        else createElfCaloriesLists (init acc ++ [last acc ++ [UtilLib.readInt str]]) strs
