module UtilLib (readInt, maximum3, sumTupleElements) where

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
