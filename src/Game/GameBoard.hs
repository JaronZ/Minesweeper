module Game.GameBoard (generateBoardWithSize, drawBoard, fillBoardCellValues, Board) where
import Game.CellState (
    CellState (Hidden, Flag, Unknown, Uncovered),
    CellValue (Empty, Bomb, Number, None),
    stateToChar,
    getValueOfState,
    setValueOfState,
    valueEquals,
    incrementValueFromState)
import Grid.Grid (Grid, drawGrid)
import Util (repeatTimes, replace, replaceWith, applyPositionBoundary)
import System.Random (randomRIO)

type Board = [[CellState]]
type BoardBombFiller = (Int, IO Board) -> (Int, Int) -> IO Board

initialCellState :: CellState
initialCellState = Hidden None

generateBoardWithSize :: (Int, Int) -> Board
generateBoardWithSize (x, y) = repeatTimes y (repeatTimes x initialCellState)

fillBoardCellValues :: BoardBombFiller
fillBoardCellValues (bombs, boardIO) = \(x, y) -> do boardWithBombs <- fillBoardWithBombs (bombs, boardIO) (x, y)
                                                     return (fillNoneCellValues boardWithBombs (0,0))

fillNoneCellValues :: Board -> (Int, Int) -> Board
fillNoneCellValues board = \(x, y) -> do let transformState = \state -> case getValueOfState state of
                                                                          None -> Just (setValueOfState state Empty)
                                                                          _ -> Nothing
                                             replacedRow = replaceWith transformState (board !! x) y
                                             replacedBoard = replace board x replacedRow
                                         if (x+1) == (length board) && (y+1) == (length (head board)) then
                                            replacedBoard
                                         else if (y+1) == (length (head board)) then
                                            fillNoneCellValues replacedBoard ((x+1), 0)
                                         else
                                            fillNoneCellValues replacedBoard (x, (y+1))

fillBoardWithBombs :: BoardBombFiller
fillBoardWithBombs (0, board) = \_ -> board
fillBoardWithBombs (bombs, boardIO) = \(x, y) -> do board <- boardIO
                                                    bombX <- randomRIO (0,(length board) - 1)
                                                    bombY <- randomRIO (0,(length (head board)) - 1)
                                                    let cellValue = getCellValueAtPosition board (bombX, bombY)
                                                    if (bombX == x && bombY == y) || valueEquals (cellValue, Bomb) then
                                                      fillBoardWithBombs (bombs, boardIO) (x, y)
                                                    else let newBoard = placeBombOnBoard board (bombX, bombY)
                                                         in fillBoardWithBombs (bombs-1, (return newBoard)) (x, y)

placeBombOnBoard :: Board -> (Int, Int) -> Board
placeBombOnBoard board = \(x, y) -> do let replacedBoardWithBombs = replace board x (replace (board !! x) y (Hidden Bomb))
                                           replacedBoardWithNumbers = incrementCells replacedBoardWithBombs ((x-1, y-1), (x+1, y+1))
                                       replacedBoardWithNumbers

incrementCells :: Board -> ((Int, Int), (Int, Int)) -> Board
incrementCells board = \(posStart, posEnd) -> do let applyBoundaries = applyPositionBoundary ((0, (length board) - 1), (0, (length (head board)) - 1))
                                                     ((fromX, fromY), (toX, toY)) = (applyBoundaries posStart, applyBoundaries posEnd)
                                                 if fromX > toX then
                                                    incrementCells board ((toX, fromY), (fromX, toY))
                                                 else
                                                   let replacedBoard = replace board fromX (incrementCellRow (board !! fromX) (fromY, toY))
                                                   in if fromX == toX then
                                                      replacedBoard
                                                   else
                                                      incrementCells replacedBoard ((fromX+1, fromY), (toX, toY))

incrementCellRow :: [CellState] -> (Int, Int) -> [CellState]
incrementCellRow row = \(from, to) -> do if from > to then
                                            incrementCellRow row (to, from)
                                         else
                                             let replacedRow = replaceWith (\state -> incrementValueFromState state) row from
                                             in if from == to then
                                                replacedRow
                                             else
                                                incrementCellRow replacedRow (from+1, to)

drawBoard :: Board -> IO ()
drawBoard board = drawGrid 0 (boardToGrid board)

boardToGrid :: Board -> Grid
boardToGrid board = map (map stateToChar) board

getCellValueAtPosition :: Board -> (Int, Int) -> CellValue
getCellValueAtPosition board = \(x, y) -> getValueOfState ((board !! x) !! y)
