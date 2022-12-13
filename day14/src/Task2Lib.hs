module Task2Lib (taskFunc) where

import Control.Monad
import Control.Monad.State
import System.Random (RandomGen, Random (random), StdGen, mkStdGen)

taskFunc :: [String] -> IO ()
taskFunc _ = do
    print $ runState threeCoinsState (mkStdGen 1)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoinsState :: State StdGen (Int, Int, Bool)
threeCoinsState = do
  a <- randomSt 
  b <- randomSt
  c <- randomSt
  return (a, b, c)
