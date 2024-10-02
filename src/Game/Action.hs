module Game.Action (performGameAction, GameAction (AClick, AFlag, AUnknown)) where
import Game.GameBoard (Board)
import Game.CellState (CellState (Hidden, Flag, Unknown, Uncovered))
import Util (replace)

type ActionFunction = Board -> (Int, Int) -> Board
data GameAction = AClick | AFlag | AUnknown

performGameAction :: GameAction -> ActionFunction
performGameAction AClick = click
performGameAction AFlag = flag
performGameAction AUnknown = unknown

click :: ActionFunction
click = performActionAt (\state -> case state of
                                    (Hidden a) -> Just (Uncovered a)
                                    _ -> Nothing)

flag :: ActionFunction
flag = performActionAt (\state -> case state of
                                    (Flag a) -> Just (Hidden a)
                                    (Hidden a) -> Just (Flag a)
                                    _ -> Nothing)

unknown :: ActionFunction
unknown = performActionAt (\state -> case state of
                                       (Unknown a) -> Just (Hidden a)
                                       (Hidden a) -> Just (Unknown a)
                                       _ -> Nothing)

performActionAt :: (CellState -> Maybe CellState) -> ActionFunction
performActionAt action = \board -> \(x, y) -> let (leftX,selectedX:rightX) = splitAt x board
                                                  (leftY,selectedY:rightY) = splitAt y selectedX
                                              in case (action selectedY) of
                                                   (Just newState) -> leftX ++ [leftY ++ (newState:rightY)] ++ rightX
                                                   Nothing -> board
