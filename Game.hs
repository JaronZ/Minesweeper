module Game where
import Grid (generateGridFrom, drawGrid, Grid)

startChar :: Char
startChar = ' '

classicGrid :: Grid
classicGrid = generateGridFrom (8,8) startChar

minesweeper :: IO ()
minesweeper = drawGrid 0 classicGrid
