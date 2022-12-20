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
  let jetPatterns = parseInputLines inputLines
  putStrLn "End position:"
  let (newPosition, newJetPatterns) = handleMultipleRocksDrops 560 [] jetPatterns
  printPosition newPosition
  putStrLn "Length position:"
  let lengthNewPosition = length newPosition
  print $ lengthNewPosition
  putStrLn "Find repeat:"
  let (repeatPosition, count) = findRepeat newJetPatterns newPosition
  putStrLn "Repeat count:"
  let repeatCount = count * 5
  print repeatCount
  putStrLn "Length of repeat:"
  let repeatLength = length repeatPosition - lengthNewPosition
  print repeatLength
  putStrLn "Modulus:"
  let modulus = (1000000000000 - 560 * 5) `mod` repeatCount
  print modulus
  putStrLn "Result:"
  let result = (1000000000000 - 560 * 5) `div` repeatCount * repeatLength + lengthNewPosition
  print result


parseInputLines :: [String] -> [Char]
parseInputLines = cycle . head

type Position = [[Bool]]

type Rock = [(Int, Int)]

initialPosition :: Position
initialPosition = []

rocks :: [Rock]
rocks =
  [ [(2, 0), (3, 0), (4, 0), (5, 0)],
    [(3, 0), (2, 1), (3, 1), (4, 1), (3, 2)],
    [(4, 0), (4, 1), (2, 2), (3, 2), (4, 2)],
    [(2, 0), (2, 1), (2, 2), (2, 3)],
    [(2, 0), (3, 0), (2, 1), (3, 1)]
  ]

printPosition :: Position -> IO ()
printPosition [] = return ()
printPosition (row : rows) = do
  let rowStr = map (\b -> if b then '#' else '.') row
  putStrLn rowStr
  printPosition rows

positionWidth :: Int
positionWidth = 7

findRepeat :: [Char] -> Position -> (Position, Int)
findRepeat jetPatterns position = findRepeatSub position newJetPatterns newPosition 1
  where
    (newPosition, newJetPatterns) = handleDrops position jetPatterns rocks

findRepeatSub :: Position -> [Char] -> Position -> Int -> (Position, Int)
findRepeatSub firstPosition jetPatterns position count = do
  if take 40 firstPosition == take 40 position
    then (position, count)
    else findRepeatSub firstPosition newJetPatterns newPosition (count + 1)
  where
    (newPosition, newJetPatterns) = handleDrops position jetPatterns rocks

findStr :: String -> String -> Maybe Int
findStr pat str = findStrHelp pat str 0
  where
    findStrHelp _ [] _ = Nothing
    findStrHelp pat s@(x : xs) n
      | pat == take (length pat) s = Just n
      | otherwise = findStrHelp pat xs (n + 1)

handleMultipleRocksDrops :: Int -> Position -> [Char] -> (Position, [Char])
handleMultipleRocksDrops count position jetPatterns =
  if count == 0
    then (position, jetPatterns)
    else handleMultipleRocksDrops (count - 1) newPosition newJetPatterns
  where
    (newPosition, newJetPatterns) = handleDrops position jetPatterns rocks

handleDrops :: Position -> [Char] -> [Rock] -> (Position, [Char])
handleDrops position jetPatterns = foldl handleDrop (position, jetPatterns)

handleDrop :: (Position, [Char]) -> Rock -> (Position, [Char])
handleDrop (position, jetPatterns) rock = (newPosition, newJetPatterns)
  where
    updatedPosition = replicate (calcRockHeight rock + 3) (replicate positionWidth False) ++ position
    (newPosition, newJetPatterns) = performDrop updatedPosition jetPatterns rock

calcRockHeight :: Rock -> Int
calcRockHeight rock = maximum ys - minimum ys + 1
  where
    ys = map snd rock

performDrop :: Position -> [Char] -> Rock -> (Position, [Char])
performDrop position (jetPattern : jetPatterns) rock =
  if rockFits position droppedRock
    then performDrop position jetPatterns droppedRock
    else (newPositionWithRock, jetPatterns)
  where
    pushedRock = if jetPattern == '<' then moveRockLeft position rock else moveRockRight position rock
    droppedRock = map (\(x, y) -> (x, y + 1)) pushedRock
    newPositionWithRock = removeEmptyRowsFromTop $ updatePosition position pushedRock

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
updatePosition position ((x, y) : coords) = updatePosition newPosition coords
  where
    newPosition = take y position ++ updatedRow : drop (y + 1) position
    row = position !! y
    updatedRow = take x row ++ True : drop (x + 1) row

removeEmptyRowsFromTop :: Position -> Position
removeEmptyRowsFromTop [] = []
removeEmptyRowsFromTop (row : rows) = if True `notElem` row then removeEmptyRowsFromTop rows else row : rows