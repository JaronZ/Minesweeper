module Grid.Grid (drawGrid, Grid) where
import Grid.Row (drawRow, drawRowNumbers, generateRow, Row)
import Util (repeatTimes)
import Data.Char (ord, chr, digitToInt)

type Grid = [Row]

altTableA :: Int
altTableA = ord 'A'

--getPosition :: String -> (Int, Int)
--getPosition cell = do col <- ord (head cell)
--                      row <- digitToInt (head cell)
--                      return (col, row)

drawGrid :: Int -> Grid -> IO ()
drawGrid _ [] = return ()
drawGrid nthRow (xs:xss) = do if nthRow == 0 then
                                do putStr "   "
                                   drawRowNumbers (1, length xss + 1)
                              else
                                return ()
                              drawRow (nthRow == 0, length xss == 0) (chr (altTableA + nthRow), xs)
                              drawGrid (nthRow + 1) xss
