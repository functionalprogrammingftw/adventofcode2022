module UtilLib (
    readInt,
    readInteger,
    maximum3,
    sumTupleElements,
    splitEvery,
    tuplify2,
    elems,
    elemsOverlap,
    every,
    countTrue,
    countTrueGrid,
    replaceNth,
    fpow,
    primeFactors,
    filterIndexed,
    anyIndexed
) where

import Data.List (any)

readInt :: String -> Int
readInt = read

readInteger :: String -> Integer
readInteger = read

maximum3 :: [Int] -> (Int, Int, Int)
maximum3 = foldl maximum3Fold (0, 0, 0)

maximum3Fold :: (Int, Int, Int) -> Int -> (Int, Int, Int)
maximum3Fold (fstMax, sndMax, trdMax) elm
    | elm > fstMax = (elm, fstMax, sndMax)
    | elm > sndMax = (fstMax, elm, sndMax)
    | elm > trdMax = (fstMax, sndMax, elm)
    | otherwise = (fstMax, sndMax, trdMax)

sumTupleElements :: (Int, Int, Int) -> Int
sumTupleElements (x, y, z) = x + y + z

splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
    where
        (first, rest) = splitAt n list

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

elems :: Eq a => [a] -> [a] -> Bool
elems [] _ = True
elems (x:xs) ys
    | x `elem` ys = elems xs ys
    | otherwise = False

elemsOverlap :: Eq a => [a] -> [a] -> Bool
elemsOverlap [] _ = False
elemsOverlap (x:xs) ys
    | x `elem` ys = True
    | otherwise = elemsOverlap xs ys

every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              y : ys -> y : every n ys
              [] -> []

countTrue :: [Bool] -> Int
countTrue list = sum $ map fromEnum list

countTrueGrid :: [[Bool]] -> Int
countTrueGrid grid = sum $ map countTrue grid

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

fpow 0 f = id
fpow n f = f . fpow (n-1) f

primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

filterIndexed f = map snd . filter (uncurry f) . enumerate

anyIndexed f = any (uncurry f) . enumerate

enumerate = zip [0..]