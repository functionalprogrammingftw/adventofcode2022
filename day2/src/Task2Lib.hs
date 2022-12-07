module Task2Lib (task2Func) where

import qualified UtilLib (readInt)

task2Func :: [String] -> IO ()
task2Func strategyLines = do
    putStrLn "Strategies: "
    print $ createTuples strategyLines
    putStrLn "Transformed strategies: "
    print $ map transformTuple $ createTuples strategyLines
    putStrLn "Scores: "
    print $ map (calcTupleScore . transformTuple) $ createTuples strategyLines
    putStrLn "Sum of scores: "
    print $ sum . map (calcTupleScore . transformTuple) $ createTuples strategyLines

createTuples :: [String] -> [(Char, Char)]
createTuples [] = []
createTuples ((c1:c2:c3:cs):xs) = (c1, c3):createTuples xs

calcTupleScore :: (Char, Char) -> Int
calcTupleScore tuple = calcShapeScore tuple + calcOutcomeScore tuple

calcShapeScore :: (Char, Char) -> Int
calcShapeScore (c, 'A') = 1
calcShapeScore (c, 'B') = 2
calcShapeScore (c, 'C') = 3

transformTuple :: (Char, Char) -> (Char, Char)
transformTuple ('A', 'X') = ('A', 'C')
transformTuple ('B', 'X') = ('B', 'A')
transformTuple ('C', 'X') = ('C', 'B')
transformTuple ('A', 'Y') = ('A', 'A')
transformTuple ('B', 'Y') = ('B', 'B')
transformTuple ('C', 'Y') = ('C', 'C')
transformTuple ('A', 'Z') = ('A', 'B')
transformTuple ('B', 'Z') = ('B', 'C')
transformTuple ('C', 'Z') = ('C', 'A')

calcOutcomeScore :: (Char, Char) -> Int
calcOutcomeScore ('A', 'A') = 3
calcOutcomeScore ('B', 'A') = 0
calcOutcomeScore ('C', 'A') = 6
calcOutcomeScore ('A', 'B') = 6
calcOutcomeScore ('B', 'B') = 3
calcOutcomeScore ('C', 'B') = 0
calcOutcomeScore ('A', 'C') = 0
calcOutcomeScore ('B', 'C') = 6
calcOutcomeScore ('C', 'C') = 3
