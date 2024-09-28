module GameBoard (generateBoardWithSize, boardToGrid, Board) where
import CellState (CellState (Hidden), CellValue (None), stateToChar)
import Grid (Grid)
import Util (repeatTimes)

type Board = [[CellState]]

initialCellState :: CellState
initialCellState = Hidden None

generateBoardWithSize :: (Int, Int) -> Board
generateBoardWithSize (x, y) = repeatTimes y (repeatTimes x initialCellState)

boardToGrid :: Board -> Grid
boardToGrid b = map (map stateToChar) b
