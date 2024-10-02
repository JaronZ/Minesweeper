module Game.Game (minesweeper) where
import Game.GameBoard (generateBoardWithSize, drawBoard, fillBoardCellValues, Board)
import Game.Action (performGameAction, GameAction)
import Parser (parseGameInput)

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
                             input <- getLine
                             case parseGameInput input of
                                (Just (action, pos)) -> do let updatedBoard = performGameAction action board pos
                                                           if input == "Y" then
                                                              return ()
                                                           else if bombs == 0 then
                                                              playGame (0, updatedBoard)
                                                           else do
                                                              filledBoard <- fillBoardCellValues (bombs, (return updatedBoard)) (0, 0)
                                                              playGame (0, filledBoard)
                                Nothing -> do putStrLn "Invalid input!"
                                              playGame (bombs, board)
