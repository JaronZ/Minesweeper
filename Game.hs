module Game (minesweeper) where
import Grid (generateGridFrom, drawGrid, Grid)

type GameConfiguration = (Int, Grid)

startChar :: Char
startChar = ' '

createGameConfiguration :: (Int, Int) -> Int -> GameConfiguration
createGameConfiguration size bombs = (bombs, generateGridFrom size startChar)

classicGridConfiguration :: GameConfiguration
classicGridConfiguration = createGameConfiguration (8, 8) 9

minesweeper :: IO ()
minesweeper = playGame classicGridConfiguration

playGame :: GameConfiguration -> IO ()
playGame (bombs, grid) = do drawGrid 0 grid
                            putStr "Enter an action and a cell (e.g. click C3): "
                            actionAndCell <- getLine
                            if actionAndCell == "Y" then
                              return ()
                            else
                              playGame (bombs, grid)
