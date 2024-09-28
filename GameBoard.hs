module GameBoard (generateBoardWithSize, drawBoard, Board) where
import CellState (CellState (Hidden), CellValue (None), stateToChar)
import Grid (Grid, drawGrid)
import Util (repeatTimes)

type Board = [[CellState]]

initialCellState :: CellState
initialCellState = Hidden None

generateBoardWithSize :: (Int, Int) -> Board
generateBoardWithSize (x, y) = repeatTimes y (repeatTimes x initialCellState)

drawBoard :: Board -> IO ()
drawBoard board = drawGrid 0 (boardToGrid board)

boardToGrid :: Board -> Grid
boardToGrid board = map (map stateToChar) board
