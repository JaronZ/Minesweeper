module CellState (
    CellState (Hidden, Flag, Unknown, Uncovered),
    CellValue (Empty, Bomb, Number, None),
    stateToChar
) where
import Data.Char (isDigit, digitToInt)

data CellState = Hidden CellValue | Flag CellValue | Unknown CellValue | Uncovered CellValue
data CellValue = Empty | Bomb | Number Int | None

stateToChar :: CellState -> Char
stateToChar (Hidden _) = ' '
stateToChar (Flag _) = 'F'
stateToChar (Unknown _) = '?'
stateToChar (Uncovered v) = valueToChar v

valueToChar :: CellValue -> Char
valueToChar Empty = '/'
valueToChar Bomb = '*'
valueToChar (Number n) = head (show n)

--charToValue :: Char -> CellValue
--charToValue c | c == '/' = Empty
--              | c == '*' = Bomb
--              | isDigit c = Number (digitToInt c)
