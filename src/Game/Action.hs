module Game.Action (performGameAction, {-performGameActionFromTo,-} GameAction (AClick, AFlag, AUnknown)) where
import Game.GameBoard (Board, BoardRow)
import Game.CellState (CellState (Hidden, Flag, Unknown, Uncovered), CellValue (Empty))
import Util (replace, applyPositionBoundary)

type ActionFunction = Board -> (Int, Int) -> Board
type ActionFromToFunction = Board -> ((Int, Int), (Int, Int)) -> Board
data GameAction = AClick | AFlag | AUnknown

performGameAction :: GameAction -> ActionFunction
performGameAction AClick = click
performGameAction AFlag = flag
performGameAction AUnknown = unknown

--performGameActionFromTo :: GameAction -> ActionFromToFunction
--performGameActionFromTo AClick = performActionFromTo click
--performGameActionFromTo AFlag = performActionFromTo flag
--performGameActionFromTo AUnknown = performActionFromTo unknown

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

--performActionFromTo :: ActionFunction -> ActionFromToFunction
--performActionFromTo action = \board -> \(posFrom, posTo) -> let applyBoundaries = applyPositionBoundary ((0, length board - 1), (0, (length $ head board) - 1))
--                                                            in unsafePerformActionFromTo action board (applyBoundaries posFrom, applyBoundaries posTo)
--
--unsafePerformActionFromTo :: ActionFunction -> ActionFromToFunction
--unsafePerformActionFromTo action = \board -> \((fromX, fromY), (toX, toY)) -> if fromX > toX then
--                                                                                 performActionFromTo action board ((toX, fromY), (fromX, toY))
--                                                                              else let updatedBoard = replace board fromX (unsafePerformActionFromToRow action (board !! fromX) (fromY, toY))
--                                                                                   in if fromX == toX then
--                                                                                      updatedBoard
--                                                                                   else
--                                                                                      unsafePerformActionFromTo action board ((fromX+1, fromY), (toX, toY))
--
--unsafePerformActionFromToRow :: ActionFunction -> BoardRow -> (Int, Int) -> BoardRow
--unsafePerformActionFromToRow action = \row -> \(from, to) -> if from > to then
--                                                                unsafePerformActionFromToRow action row (to, from)
--                                                             else let [updatedRow] = action [row] (0, from)
--                                                                  in if from == to then
--                                                                      updatedRow
--                                                                  else
--                                                                      unsafePerformActionFromToRow action updatedRow (from+1, to)
