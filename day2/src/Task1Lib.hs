module Task1Lib (task1Func) where

import qualified UtilLib (readInt)

task1Func :: [String] -> IO ()
task1Func strategyLines = do
    putStrLn "Strategies: "
    print $ createTuples strategyLines
    putStrLn "Scores: "
    print $ map calcTupleScore $ createTuples strategyLines
    putStrLn "Sum of scores: "
    print $ sum . map calcTupleScore $ createTuples strategyLines

createTuples :: [String] -> [(Char, Char)]
createTuples [] = []
createTuples ((c1:c2:c3:cs):xs) = (c1, c3):createTuples xs

calcTupleScore :: (Char, Char) -> Int
calcTupleScore tuple = calcShapeScore tuple + calcOutcomeScore tuple

calcShapeScore :: (Char, Char) -> Int
calcShapeScore (c, 'X') = 1
calcShapeScore (c, 'Y') = 2
calcShapeScore (c, 'Z') = 3

calcOutcomeScore :: (Char, Char) -> Int
calcOutcomeScore ('A', 'X') = 3
calcOutcomeScore ('B', 'X') = 0
calcOutcomeScore ('C', 'X') = 6
calcOutcomeScore ('A', 'Y') = 6
calcOutcomeScore ('B', 'Y') = 3
calcOutcomeScore ('C', 'Y') = 0
calcOutcomeScore ('A', 'Z') = 0
calcOutcomeScore ('B', 'Z') = 6
calcOutcomeScore ('C', 'Z') = 3
