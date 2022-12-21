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
  putStrLn "Input lines:"
  print inputLines

data RobotPrice = RobotPrice {
  orePrice :: Int,
  clayPrice :: Int,
  obsidianPrice :: Int
} deriving (Show, Eq)

data Blueprint = Blueprint {
  oreRobotPrice :: RobotPrice,
  clayRobotPrice :: RobotPrice,
  obsidianRobotPrice :: RobotPrice,
  geodeRobotPrice :: RobotPrice
} deriving (Show, Eq)

data RobotConfig = RobotConfig {
  oreRobots :: Int,
  clayRobots :: Int,
  obsidianRobots :: Int,
  geodeRobots :: Int
} deriving (Show, Eq)

data Resources = Resources {
  ore :: Int,
  clay :: Int,
  obsidian :: Int,
  geode :: Int
} deriving (Show, Eq)

data Inventory = Inventory {
  resources :: Resources,
  robotConfig :: RobotConfig
} deriving (Show, Eq)
