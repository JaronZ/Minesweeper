module Parser (parseGameInput) where
import Data.Char (ord, digitToInt)
import Util (altTableA)
import Game.Action (GameAction (AClick, AFlag, AUnknown))

parseGameInput :: String -> Maybe (GameAction, (Int, Int))
parseGameInput input = do let inputWords = words input
                          if (length inputWords) < 2 then
                            Nothing
                          else let [left, right] = inputWords
                               in case parseGameAction left of
                                 (Just action) -> Just (action, parseGameCell right)
                                 Nothing -> Nothing

parseGameAction :: String -> Maybe GameAction
parseGameAction "click" = Just AClick
parseGameAction "flag" = Just AFlag
parseGameAction "unknown" = Just AUnknown
parseGameAction _ = Nothing

parseGameCell :: String -> (Int, Int)
parseGameCell input = do let col = (ord (head input)) - altTableA
                             row = (digitToInt (head (tail input))) - 1
                         (col, row)
