module UtilLib (readInt, maximum3, sumTupleElements, splitEvery, tuplify2, elems, elemsOverlap, every) where

readInt :: String -> Int
readInt = read

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