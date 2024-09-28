module Grid where
import Row (drawRow, drawRowNumbers, generateRow, Row)
import Data.Char (ord, chr)

type Grid = [Row]

altTableA :: Int
altTableA = ord 'A'

generateGridFrom :: (Int, Int) -> Char -> Grid
generateGridFrom (x, y) = \startChar -> take y (repeat (generateRow x startChar))

drawGrid :: Int -> Grid -> IO ()
drawGrid _ [] = return ()
drawGrid nthRow (xs:xss) = do if nthRow == 0 then
                                do putStr "   "
                                   drawRowNumbers (1, length xss + 1)
                              else
                                return ()
                              drawRow (nthRow == 0, length xss == 0) (chr (altTableA + nthRow), xs)
                              drawGrid (nthRow + 1) xss
