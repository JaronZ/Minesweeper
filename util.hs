module Util (repeatTimes) where

repeatTimes :: Int -> a -> [a]
repeatTimes n x = take n (repeat x)
