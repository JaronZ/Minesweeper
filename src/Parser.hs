module Parser (parseGameInput) where
import Data.Char (ord, digitToInt, toUpper, toLower, isDigit)
import Util (altTableA)
import Game.Action (GameAction (AClick, AFlag, AUnknown))

parseGameInput :: String -> Maybe (GameAction, (Int, Int))
parseGameInput input = do let inputWords = words input
                          if length inputWords < 2 then
                            Nothing
                          else let [left, right] = inputWords
                               in case (parseGameAction $ map toLower left, parseGameCell right) of
                                 (Just action, Just cell) -> Just (action, cell)
                                 _ -> Nothing

parseGameAction :: String -> Maybe GameAction
parseGameAction "click" = Just AClick
parseGameAction "flag" = Just AFlag
parseGameAction "unknown" = Just AUnknown
parseGameAction _ = Nothing

parseGameCell :: String -> Maybe (Int, Int)
parseGameCell input = do if (length input) /= 2 then
                            Nothing
                         else let col = toUpper $ head input
                                  row = head $ tail input
                              in if (notElem col ['A'..'Z']) || (not $ isDigit row) then
                                    Nothing
                                 else
                                    Just (ord col - altTableA, digitToInt row - 1)
