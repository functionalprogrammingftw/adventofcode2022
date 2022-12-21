{-# LANGUAGE NamedFieldPuns #-}

module Task1Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import Data.List (elemIndex, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, split, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set (Set, delete, empty, fromList, insert, isSubsetOf, singleton, union)
import UtilLib (countTrueGrid, every, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  putStrLn "Blueprints:"
  let blueprints = parseInputLines inputLines
  print blueprints

parseInputLines :: [String] -> [Blueprint]
parseInputLines = map parseInputLine

parseInputLine :: String -> Blueprint
parseInputLine str = Blueprint {blueprintId, oreRobotPrice, clayRobotPrice, obsidianRobotPrice, geodeRobotPrice}
  where
    [startStr, endStr] = splitOn ": " str
    blueprintId = UtilLib.readInt $ drop 10 startStr
    [oreRobotPriceStr, clayRobotPriceStr, obsidianRobotPriceStr, geodeRobotPriceStr] = splitOn ". " str
    oreRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " (drop 21 oreRobotPriceStr),
          clayPrice = 0,
          obsidianPrice = 0
        }
    clayRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " (drop 22 clayRobotPriceStr),
          clayPrice = 0,
          obsidianPrice = 0
        }
    obsidianRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " (drop 26 obsidianRobotPriceStr),
          clayPrice = 0,
          obsidianPrice = 0
        }
    geodeRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " (drop 23 geodeRobotPriceStr),
          clayPrice = 0,
          obsidianPrice = 0
        }

-- Model

data RobotPrice = RobotPrice
  { orePrice :: Int,
    clayPrice :: Int,
    obsidianPrice :: Int
  }
  deriving (Show, Eq)

data Blueprint = Blueprint
  { blueprintId :: Int,
    oreRobotPrice :: RobotPrice,
    clayRobotPrice :: RobotPrice,
    obsidianRobotPrice :: RobotPrice,
    geodeRobotPrice :: RobotPrice
  }
  deriving (Show, Eq)

data RobotConfig = RobotConfig
  { oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    geodeRobots :: Int
  }
  deriving (Show, Eq)

data Resources = Resources
  { ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geode :: Int
  }
  deriving (Show, Eq)

data Inventory = Inventory
  { resources :: Resources,
    robotConfig :: RobotConfig
  }
  deriving (Show, Eq)
