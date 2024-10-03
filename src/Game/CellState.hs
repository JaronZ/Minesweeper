module Game.CellState (
    CellState (Hidden, Flag, Unknown, Uncovered),
    CellValue (Empty, Bomb, Number, None),
    stateToChar,
    getValueOfState,
    setValueOfState,
    valueEquals,
    stateEquals,
    stateEqualsIgnoreValue,
    incrementValueFromState
) where
import Data.Char (isDigit, digitToInt)

data CellState = Hidden CellValue | Flag CellValue | Unknown CellValue | Uncovered CellValue
data CellValue = Empty | Bomb | Number Int | None

getValueOfState :: CellState -> CellValue
getValueOfState (Hidden v) = v
getValueOfState (Flag v) = v
getValueOfState (Unknown v) = v
getValueOfState (Uncovered v) = v

setValueOfState :: CellState -> CellValue -> CellState
setValueOfState (Hidden _) v = Hidden v
setValueOfState (Flag _) v = Flag v
setValueOfState (Unknown _) v = Unknown v
setValueOfState (Uncovered _) v = Uncovered v

incrementValueFromState :: CellState -> Maybe CellState
incrementValueFromState state = case incrementValue (getValueOfState state) of
                                  Nothing -> Nothing
                                  (Just iv) -> Just (setValueOfState state iv)

stateToChar :: CellState -> Char
stateToChar (Hidden _) = ' '
stateToChar (Flag _) = 'F'
stateToChar (Unknown _) = '?'
stateToChar (Uncovered v) = valueToChar v

incrementValue :: CellValue -> Maybe CellValue
incrementValue (Number n) = Just (Number (n+1))
incrementValue (None) = Just (Number 1)
incrementValue _ = Nothing

valueToChar :: CellValue -> Char
valueToChar Empty = '/'
valueToChar Bomb = '*'
valueToChar None = 'E'
valueToChar (Number n) = head (show n)

valueEquals :: (CellValue, CellValue) -> Bool
valueEquals (Empty, Empty) = True
valueEquals (Bomb, Bomb) = True
valueEquals (Number _, Number _) = True
valueEquals (None, None) = True
valueEquals _ = False

stateEquals :: (CellState, CellState) -> Bool
stateEquals (Hidden a, Hidden b) = valueEquals (a, b)
stateEquals (Flag a, Flag b) = valueEquals (a, b)
stateEquals (Unknown a, Unknown b) = valueEquals (a, b)
stateEquals (Uncovered a, Uncovered b) = valueEquals (a, b)
stateEquals _ = False

stateEqualsIgnoreValue :: (CellState, CellState) -> Bool
stateEqualsIgnoreValue (Hidden _, Hidden _) = True
stateEqualsIgnoreValue (Flag _, Flag _) = True
stateEqualsIgnoreValue (Unknown _, Unknown _) = True
stateEqualsIgnoreValue (Uncovered _, Uncovered _) = True
stateEqualsIgnoreValue _ = False

--charToValue :: Char -> CellValue
--charToValue c | c == '/' = Empty
--              | c == '*' = Bomb
--              | isDigit c = Number (digitToInt c)
