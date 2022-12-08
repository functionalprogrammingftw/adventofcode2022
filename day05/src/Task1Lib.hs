module Task1Lib (taskFunc) where

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
moveSeveral stacks [0, _, _] = stacks
moveSeveral stacks [count, from, to] = moveSeveral newStacks [count - 1, from, to]
    where newStacks = moveOne stacks from to

moveOne :: [[Char]] -> Int -> Int -> [[Char]]
moveOne stacks from to = [ if idx == fromIdx then tail xs else if idx == toIdx then elmToMove:xs else xs | (xs, idx) <- zip stacks [0..] ]
    where elmToMove = head $ stacks !! fromIdx
          fromIdx = from - 1
          toIdx = to - 1