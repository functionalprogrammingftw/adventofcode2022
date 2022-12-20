module Task2Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import Data.List (elemIndex, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set (Set, delete, empty, fromList, insert, isSubsetOf, singleton, union)
import UtilLib (countTrueGrid, every, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  putStrLn "Jet patterns:"
  let jetPatterns = parseInputLines inputLines
  print $ take 100 jetPatterns
  -- putStrLn "End position:"
  handleDropsAndPrint 1000 1000 jetPatterns rocks [] 0
  -- printPosition position

parseInputLines :: [String] -> String
parseInputLines = cycle . head

type Position = [[Bool]]

type Rock = [(Int, Int)]

initialPosition :: Position
initialPosition = []

rocks :: [Rock]
rocks =
  cycle
    [ [(2, 0), (3, 0), (4, 0), (5, 0)],
      [(3, 0), (2, 1), (3, 1), (4, 1), (3, 2)],
      [(4, 0), (4, 1), (2, 2), (3, 2), (4, 2)],
      [(2, 0), (2, 1), (2, 2), (2, 3)],
      [(2, 0), (3, 0), (2, 1), (3, 1)]
    ]

printPosition :: Position -> IO ()
printPosition [] = return ()
printPosition (row:rows) = do
  let rowStr = map (\b -> if b then '#' else '.') row
  putStrLn rowStr
  printPosition rows

positionWidth :: Int
positionWidth = 7

handleDropsAndPrint :: Int -> Int -> [Char] -> [Rock] -> Position -> Int -> IO ()
handleDropsAndPrint 0 _ _ _ _ _ = return ()
handleDropsAndPrint outerStepCount innerStepCount jetPatterns rocks position rowsRemoved = do
  print $ show outerStepCount ++ " " ++ show (length newPosition + totalRowsRemoved)
  handleDropsAndPrint (outerStepCount - 1) innerStepCount newJetPatterns newRocks newPosition totalRowsRemoved
  where (newPosition, newJetPatterns, newRocks, newRowsRemoved) = handleDrops innerStepCount jetPatterns rocks position
        totalRowsRemoved = rowsRemoved + newRowsRemoved

handleDrops :: Int -> [Char] -> [Rock] -> Position -> (Position, [Char], [Rock], Int)
handleDrops stepCount jetPatterns rocks position = foldl handleDrop (position, jetPatterns, rocks, 0) [1 .. stepCount]

handleDrop :: (Position, [Char], [Rock], Int) -> Int -> (Position, [Char], [Rock], Int)
handleDrop (position, jetPatterns, rocks, rowsRemoved) step = (newPosition, newJetPatterns, newRocks, rowsRemoved + newRowsRemoved)
  where
    newRocks = tail rocks
    nextRock = head rocks
    updatedPosition = replicate (calcRockHeight nextRock + 3) (replicate positionWidth False) ++ position
    (newPosition, newJetPatterns, newRowsRemoved) = performDrop updatedPosition jetPatterns nextRock

calcRockHeight :: Rock -> Int
calcRockHeight rock = maximum ys - minimum ys + 1
  where
    ys = map snd rock

performDrop :: Position -> [Char] -> Rock -> (Position, [Char], Int)
performDrop position (jetPattern : jetPatterns) rock =
  if rockFits position droppedRock
    then performDrop position jetPatterns droppedRock
    else (topOfNewPositionWithRock, jetPatterns, rowsRemoved)
  where
    pushedRock = if jetPattern == '<' then moveRockLeft position rock else moveRockRight position rock
    droppedRock = map (\(x, y) -> (x, y + 1)) pushedRock
    newPositionWithRock = removeEmptyRowsFromTop $ updatePosition position pushedRock
    (topOfNewPositionWithRock, rowsRemoved) = removeRowsFromBottom newPositionWithRock

moveRockLeft :: Position -> Rock -> Rock
moveRockLeft position rock = if rockFits position movedRock then movedRock else rock
  where
    movedRock = map (\(x, y) -> (x - 1, y)) rock

moveRockRight :: Position -> Rock -> Rock
moveRockRight position rock = if rockFits position movedRock then movedRock else rock
  where
    movedRock = map (\(x, y) -> (x + 1, y)) rock

rockFits :: Position -> Rock -> Bool
rockFits position rock = not (any (\(x, y) -> x < 0 || x >= positionWidth || y >= length position || position !! y !! x) rock)

updatePosition :: Position -> Rock -> Position
updatePosition position [] = position
updatePosition position ((x, y):coords) = updatePosition newPosition coords
  where newPosition = take y position ++ updatedRow : drop (y + 1) position
        row = position !! y
        updatedRow = take x row ++ True : drop (x + 1) row

removeEmptyRowsFromTop :: Position -> Position
removeEmptyRowsFromTop [] = []
removeEmptyRowsFromTop (row:rows) = if True `notElem` row then removeEmptyRowsFromTop rows else row:rows

removeRowsFromBottom :: Position -> (Position, Int)
removeRowsFromBottom position = if positionLength > 100 then (take 100 position, positionLength - 100) else (position, 0)
  where positionLength = length position