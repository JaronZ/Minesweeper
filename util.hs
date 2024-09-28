module Util where

repeatTimes :: Int -> a -> [a]
repeatTimes n x = take n (repeat x)
