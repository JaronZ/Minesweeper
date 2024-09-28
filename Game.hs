module Game (minesweeper) where
import GameBoard (generateBoardWithSize, boardToGrid, Board)
import Grid (drawGrid)

type GameConfiguration = (Int, Board)

createGameConfiguration :: (Int, Int) -> Int -> GameConfiguration
createGameConfiguration size bombs = (bombs, generateBoardWithSize size)

classicGridConfiguration :: GameConfiguration
classicGridConfiguration = createGameConfiguration (8, 8) 9

minesweeper :: IO ()
minesweeper = playGame classicGridConfiguration

playGame :: GameConfiguration -> IO ()
playGame (bombs, board) = do drawGrid 0 (boardToGrid board)
                             putStr "Enter an action and a cell (e.g. click C3): "
                             actionAndCell <- getLine
                             if actionAndCell == "Y" then
                               return ()
                             else
                               playGame (bombs, board)
