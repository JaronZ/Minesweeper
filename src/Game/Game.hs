module Game.Game (minesweeper) where
import Game.GameBoard (
    generateBoardWithSize,
    drawBoard,
    fillBoardCellValues,
    isUncoveredBomb,
--    isUncoveredEmpty,
    allBombsFlaggedOrHiddenAndOtherCellsUncovered,
    Board)
import Game.Action (performGameAction, {-performGameActionFromTo,-} GameAction)
import Parser (parseGameInput)

type GameConfiguration = (Int, Board)
data BoardCheckResult = LoseCondition | WinCondition | BoardUpdate Board

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
                                (Just (action, pos)) -> let updatedBoard = performGameAction action board pos
                                                        in case boardCheck updatedBoard pos of
                                                             LoseCondition -> do drawBoard board
                                                                                 putStrLn "Game Over!"
                                                             WinCondition -> do drawBoard board
                                                                                putStrLn "You Win!"
                                                             (BoardUpdate checkedBoard) -> do if bombs == 0 then
                                                                                                 playGame (0, checkedBoard)
                                                                                              else do
                                                                                                 filledBoard <- fillBoardCellValues (bombs, (return checkedBoard)) (0, 0)
                                                                                                 playGame (0, filledBoard)
                                Nothing -> do putStrLn "Invalid input!"
                                              playGame (bombs, board)

boardCheck :: Board -> (Int, Int) -> BoardCheckResult
boardCheck board pos = do if isUncoveredBomb board pos then
                              LoseCondition
                           else if allBombsFlaggedOrHiddenAndOtherCellsUncovered board then
                              WinCondition
                           else BoardUpdate board
