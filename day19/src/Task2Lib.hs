{-# LANGUAGE NamedFieldPuns #-}

module Task2Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import qualified Data.HashSet (HashSet, delete, empty, fromList, insert, isSubsetOf, member, singleton, union)
import Data.List (any, elemIndex, foldl', insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, split, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (catMaybes, fromJust)
import UtilLib (anyIndexed, countTrueGrid, every, filterIndexed, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  let blueprints = take 3 $ parseInputLines inputLines
  putStrLn "Blueprints:"
  print blueprints
  -- let blueprint = blueprints !! 0
  -- putStrLn "Blueprint:"
  -- print blueprint
  -- let inventories = calcBlueprintInventories 32 32 blueprint
  -- putStrLn "Blueprint 1 inventories after minutes:"
  -- print inventories
  -- putStrLn "Blueprint 1 inventory count after minutes:"
  -- print $ length inventories
  -- putStrLn "Geode inventory count:"
  -- print $ length $ filter (\i -> geodes i > 0) inventories
  -- putStrLn "Max geode inventory count:"
  -- print $ maximum $ map geodes inventories
  -- putStrLn "maxMinGeodes:"
  -- print $ maximum $ map (calcMinGeodes blueprint 0) inventories

  multiplied <- calcBlueprintsMaxGeodes 32 blueprints
  putStrLn "Max geodes multiplied:"
  print multiplied

parseInputLines :: [String] -> [Blueprint]
parseInputLines = map parseInputLine

parseInputLine :: String -> Blueprint
parseInputLine str = Blueprint {blueprintId, oreRobotPrice, clayRobotPrice, obsidianRobotPrice, geodeRobotPrice}
  where
    [startStr, endStr] = splitOn ": " str
    blueprintId = UtilLib.readInt $ drop 10 startStr
    [oreRobotPriceStr, clayRobotPriceStr, obsidianRobotPriceStr, geodeRobotPriceStr] = splitOn ". " endStr
    oreRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " $ drop 21 oreRobotPriceStr,
          clayPrice = 0,
          obsidianPrice = 0
        }
    clayRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " $ drop 22 clayRobotPriceStr,
          clayPrice = 0,
          obsidianPrice = 0
        }
    obsidianRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " $ drop 26 obsidianRobotPriceStr,
          clayPrice = UtilLib.readInt $ head $ splitOn " " $ last $ splitOn " ore and " obsidianRobotPriceStr,
          obsidianPrice = 0
        }
    geodeRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " $ drop 23 geodeRobotPriceStr,
          clayPrice = 0,
          obsidianPrice = UtilLib.readInt $ head $ splitOn " " $ last $ splitOn " ore and " geodeRobotPriceStr
        }

initialInventory :: Inventory
initialInventory =
  Inventory
    { oreRobots = 1,
      clayRobots = 0,
      obsidianRobots = 0,
      ore = 0,
      clay = 0,
      obsidian = 0,
      geodes = 0
    }

calcBlueprintsMaxGeodes :: Int -> [Blueprint] -> IO Int
calcBlueprintsMaxGeodes minutes [] = return 1
calcBlueprintsMaxGeodes minutes (blueprint : blueprints) = do
  factor1 <- calcBlueprintMaxGeodes minutes blueprint
  factor2 <- calcBlueprintsMaxGeodes minutes blueprints
  return $ factor1 * factor2

calcBlueprintMaxGeodes :: Int -> Blueprint -> IO Int
calcBlueprintMaxGeodes minutes blueprint = do
  let maximumGeodes = maximum (map geodes $ calcBlueprintInventories minutes 32 blueprint)
  putStrLn ("Blueprint " ++ show (blueprintId blueprint) ++ " max geodes:")
  print maximumGeodes
  return maximumGeodes

calcBlueprintInventories :: Int -> Int -> Blueprint -> [Inventory]
calcBlueprintInventories minutes totalMinutes blueprint =
  foldl' (handleMinuteInventoriesAndPrune blueprint) [initialInventory] [minutes + diff - 1, minutes + diff - 2 .. diff]
  where
    diff = totalMinutes - minutes

handleMinuteInventoriesAndPrune :: Blueprint -> [Inventory] -> Int -> [Inventory]
handleMinuteInventoriesAndPrune blueprint inventories minutesLeft = newestInventories
  where
    newInventories = handleMinuteInventories blueprint inventories minutesLeft
    newestInventories = prune blueprint newInventories minutesLeft

prune :: Blueprint -> [Inventory] -> Int -> [Inventory]
prune blueprint inventories minutesLeft = foldr pruneFold [] newInventories
  where maxMinGeodes = maximum $ map (calcMinGeodes blueprint minutesLeft) inventories
        newInventories = filter (maxGeodesGreaterThanOrEqual blueprint minutesLeft maxMinGeodes) inventories

pruneFold :: Inventory -> [Inventory] -> [Inventory]
pruneFold inventory inventories
  | betterExists = inventories
  | otherwise = inventory : filteredInventories
  where
    betterExists = any (fstInventoryWorse inventory) inventories
    filteredInventories = filter (not . fstInventoryBetter inventory) inventories

fstInventoryWorse :: Inventory -> Inventory -> Bool
fstInventoryWorse fstInventory sndInventory =
  ore fstInventory <= ore sndInventory
    && clay fstInventory <= clay sndInventory
    && obsidian fstInventory <= obsidian sndInventory
    && geodes fstInventory <= geodes sndInventory
    && oreRobots fstInventory <= oreRobots sndInventory
    && clayRobots fstInventory <= clayRobots sndInventory
    && obsidianRobots fstInventory <= obsidianRobots sndInventory

fstInventoryBetter :: Inventory -> Inventory -> Bool
fstInventoryBetter fstInventory sndInventory =
  ore fstInventory >= ore sndInventory
    && clay fstInventory >= clay sndInventory
    && obsidian fstInventory >= obsidian sndInventory
    && geodes fstInventory >= geodes sndInventory
    && oreRobots fstInventory >= oreRobots sndInventory
    && clayRobots fstInventory >= clayRobots sndInventory
    && obsidianRobots fstInventory >= obsidianRobots sndInventory

calcMinGeodes :: Blueprint -> Int -> Inventory -> Int
calcMinGeodes blueprint 0 inventory = geodes inventory
calcMinGeodes blueprint 1 inventory = geodes inventory
calcMinGeodes blueprint minutesLeft inventory =
  geodes inventory
    + if calcCanBuyGeodeRobotNextRound blueprint inventory (minutesLeft - 1) then minutesLeft - 1 else 0

maxGeodesGreaterThanOrEqual :: Blueprint -> Int -> Int -> Inventory -> Bool
maxGeodesGreaterThanOrEqual blueprint minutesLeft maxMinGeodes inventory = calcMaxGeodes blueprint minutesLeft inventory >= maxMinGeodes

calcMaxGeodes :: Blueprint -> Int -> Inventory -> Int
calcMaxGeodes blueprint 0 inventory = geodes inventory
calcMaxGeodes blueprint 1 inventory = geodes inventory
calcMaxGeodes blueprint minutesLeft inventory = geodes inventory + calcMaxFutureGeodes blueprint inventory firstPossBuyRound
  where
    firstPossBuyRound = if calcCanBuyGeodeRobotNextRound blueprint inventory (minutesLeft - 1) then minutesLeft - 1 else minutesLeft - 2

calcMaxFutureGeodes :: Blueprint -> Inventory -> Int -> Int
calcMaxFutureGeodes blueprint inventory 0 = 0
calcMaxFutureGeodes blueprint inventory minutesLeft = minutesLeft + calcMaxFutureGeodes blueprint inventory (minutesLeft - 1)

calcCanBuyGeodeRobotNextRound :: Blueprint -> Inventory -> Int -> Bool
calcCanBuyGeodeRobotNextRound blueprint inventory minutesLeft = case buyResult of
  Nothing -> False
  _ -> True
  where
    buyResult = buyRobot (geodeRobotPrice blueprint) (geodeRobotIncrementer (minutesLeft - 1)) inventory

handleMinuteInventories :: Blueprint -> [Inventory] -> Int -> [Inventory]
handleMinuteInventories blueprint [] _ = []
handleMinuteInventories blueprint (inventory : inventories) minutesLeft =
  newInventories ++ newestInventories
  where
    newInventories = handleMinuteInventory blueprint inventory minutesLeft
    newestInventories = handleMinuteInventories blueprint inventories minutesLeft

handleMinuteInventory :: Blueprint -> Inventory -> Int -> [Inventory]
handleMinuteInventory blueprint inventory minutesLeft = map (produce inventory) boughtRobotsInventories
  where
    boughtRobotsInventories =
      inventory
        : catMaybes
          [ buyRobot (oreRobotPrice blueprint) oreRobotIncrementer inventory,
            buyRobot (clayRobotPrice blueprint) clayRobotIncrementer inventory,
            buyRobot (obsidianRobotPrice blueprint) obsidianRobotIncrementer inventory,
            buyRobot (geodeRobotPrice blueprint) (geodeRobotIncrementer minutesLeft) inventory
          ]

buyRobot :: RobotPrice -> (Inventory -> Inventory) -> Inventory -> Maybe Inventory
buyRobot robotPrice robotIncrementer inventory =
  if ore newInventory >= 0 && clay newInventory >= 0 && obsidian newInventory >= 0
    then Just newInventory
    else Nothing
  where
    newInventory =
      robotIncrementer
        inventory
          { ore = ore inventory - orePrice robotPrice,
            clay = clay inventory - clayPrice robotPrice,
            obsidian = obsidian inventory - obsidianPrice robotPrice
          }

produce :: Inventory -> Inventory -> Inventory
produce originalInventory inventory =
  inventory
    { ore = ore inventory + oreRobots originalInventory,
      clay = clay inventory + clayRobots originalInventory,
      obsidian = obsidian inventory + obsidianRobots originalInventory
    }

oreRobotIncrementer :: Inventory -> Inventory
oreRobotIncrementer inventory = inventory {oreRobots = oreRobots inventory + 1}

clayRobotIncrementer :: Inventory -> Inventory
clayRobotIncrementer inventory = inventory {clayRobots = clayRobots inventory + 1}

obsidianRobotIncrementer :: Inventory -> Inventory
obsidianRobotIncrementer inventory = inventory {obsidianRobots = obsidianRobots inventory + 1}

geodeRobotIncrementer :: Int -> Inventory -> Inventory
geodeRobotIncrementer minutesLeft inventory = inventory {geodes = geodes inventory + minutesLeft}

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

data Inventory = Inventory
  { oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geodes :: Int
  }
  deriving (Show, Eq)
