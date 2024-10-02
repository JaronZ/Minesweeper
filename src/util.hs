module Util (repeatTimes, replace, replaceWith, applyPositionBoundary, altTableA) where
import Data.Char (ord)

repeatTimes :: Int -> a -> [a]
repeatTimes n x = take n (repeat x)

replace :: [a] -> Int -> a -> [a]
replace xs i a = let (left,_:right) = splitAt i xs
                 in left ++ [a] ++ right

replaceWith :: (a -> Maybe a) -> [a] -> Int -> [a]
replaceWith f = \xs i -> let (left,selected:right) = splitAt i xs
                         in case f selected of
                              (Just a) -> left ++ [a] ++ right
                              Nothing -> xs

applyPositionBoundary :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int)
applyPositionBoundary (xBounds, yBounds) = \(x, y) -> (applyIntBoundary xBounds x, applyIntBoundary yBounds y)

applyIntBoundary :: (Int, Int) -> Int -> Int
applyIntBoundary (minN, maxN) = \n -> min maxN (max minN n)

altTableA :: Int
altTableA = ord 'A'
