import Game.Game (minesweeper)
import Data.Char (toUpper)

main :: IO ()
main = do minesweeper
          putStr "Do you want to play again? (Y/N): "
          {-
          getChar is not used here because of a bug in Haskell on Windows.
          using getChar would make it so the game can only be played twice.
          -}
          playAgain <- getLine
          if toUpper (head playAgain) == 'Y' then
            main
          else
            return ()
