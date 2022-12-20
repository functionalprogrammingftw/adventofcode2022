{-# LANGUAGE NamedFieldPuns #-}

module Task1Lib (taskFunc) where

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
  putStrLn "Jet pattern:"
  let jetPattern = take 100 $ parseInputLines inputLines
  print jetPattern
  putStrLn "End position:"
  let (position, _, _) = handleDrops 2022 jetPattern rocks
  print position

parseInputLines :: [String] -> String
parseInputLines = cycle . head

type Position = [[Bool]]

type Rock = [(Int, Int)]

initialPosition :: Position
initialPosition = []

rocks :: [Rock]
rocks = cycle
  [ [(0, 2), (0, 2), (0, 3), (0, 4)],
    [(0, 3), (1, 2), (1, 3), (1, 4), (2, 3)],
    [(0, 4), (1, 4), (2, 2), (2, 3), (2, 4)],
    [(0, 2), (1, 2), (2, 2), (3, 2)],
    [(0, 2), (0, 3), (1, 2), (1, 3)]
  ]

positionWidth :: Int
positionWidth = 7

handleDrops :: Int -> [Char] -> [Rock] -> (Position, [Char], [Rock])
handleDrops stepCount jetPattern rocks = foldl handleDrop ([], jetPattern, rocks) [1 .. stepCount]

handleDrop :: (Position, [Char], [Rock]) -> Int -> (Position, [Char], [Rock])
handleDrop (position, jetPattern, rocks) step = (newPosition, newJetPatterns, newRocks)
  where newRocks = tail rocks
        nextRock = head rocks
        updatedPosition = replicate (calcRockHeight nextRock + 3) (replicate positionWidth False) ++ position
        (newPosition, newJetPatterns) = performDrop updatedPosition jetPattern nextRock

calcRockHeight :: Rock -> Int
calcRockHeight rock = maximum ys - minimum ys
  where ys = map snd rock

performDrop :: Position -> [Char] -> Rock -> (Position, [Char])
performDrop position jetPattern rock = (position, jetPattern)