module Task2Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    let [inputLinesStart, inputLinesEnd] = splitOn [""] inputLines
    putStrLn "Stacks:"
    let stacks = parseInputLinesStart inputLinesStart
    print stacks
    putStrLn "Commands:"
    let commands = parseInputLinesEnd inputLinesEnd
    print commands
    putStrLn "Result stacks:"
    let resultStacks = executeCommands stacks commands
    print resultStacks
    putStrLn "Result:"
    let result = [ x | x:xs <- resultStacks ]
    print result

executeCommands :: [[Char]] -> [[Int]] -> [[Char]]
executeCommands = foldl moveSeveral

parseInputLinesStart :: [String] -> [[Char]]
parseInputLinesStart inputLinesStart = createStacks $ map splitStringIntoCharLists $ init inputLinesStart

parseInputLinesEnd :: [String] -> [[Int]]
parseInputLinesEnd = map parseInputLineEnd

parseInputLineEnd :: String -> [Int]
parseInputLineEnd inputLineEnd =
    map UtilLib.readInt [wordList !! 1, wordList !! 3, wordList !! 5]
    where wordList = words inputLineEnd

splitStringIntoCharLists :: String -> [Char]
splitStringIntoCharLists (c1:c2:str) =
    c2:every 4 str

createStacks :: [[Char]] -> [[Char]]
createStacks [] = []
createStacks charLists =
    [ x | x:_ <- charLists, x /= ' ' ]:createStacks [ xs | _:xs <- charLists, xs /= [] ]

moveSeveral :: [[Char]] -> [Int] -> [[Char]]
moveSeveral stacks [count, from, to] = [ if idx == fromIdx then newFromStack else if idx == toIdx then newToStack else stack | (stack, idx) <- zip stacks [0..] ]
    where newToStack = take count (stacks !! fromIdx) ++ (stacks !! toIdx)
          newFromStack = drop count $ stacks !! fromIdx
          fromIdx = from - 1
          toIdx = to - 1