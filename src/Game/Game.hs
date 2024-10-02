module Game.Game (minesweeper) where
import Game.GameBoard (generateBoardWithSize, drawBoard, fillBoardCellValues, Board)

type GameConfiguration = (Int, Board)

createGameConfiguration :: (Int, Int) -> Int -> GameConfiguration
createGameConfiguration size bombs = (bombs, generateBoardWithSize size)

classicGridConfiguration :: GameConfiguration
classicGridConfiguration = createGameConfiguration (8, 8) 9

minesweeper :: IO ()
minesweeper = playGame classicGridConfiguration

playGame :: GameConfiguration -> IO ()
playGame (bombs, board) = do drawBoard board
                             putStr "Enter an action and a cell (e.g. click C3): "
                             next <- getLine
                             if next == "Y" then
                               return ()
                             else if bombs == 0 then
                               playGame (0, board)
                             else do
                               filledBoard <- fillBoardCellValues (bombs, (return board)) (0, 0)
                               playGame (0, filledBoard)
