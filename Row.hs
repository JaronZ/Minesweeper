module Row (drawRow, drawRowNumbers, generateRow, Row) where
import Util (repeatTimes)

type Row = [Char]

generateRow :: Int -> Char -> Row
generateRow = repeatTimes

drawRowNumbers :: (Int, Int) -> IO ()
drawRowNumbers (current, limit) = do putStr "   "
                                     putStr (show current)
                                     putStr "  "
                                     if current >= limit then
                                       putStrLn ""
                                     else
                                       drawRowNumbers (current + 1, limit)

drawRow :: (Bool, Bool) -> (Char, Row) -> IO ()
drawRow (False, lastRow) =
    \(label, xs) -> do drawRowLabel label
                       drawRowMiddle True xs
                       putStr "   "
                       drawRowBottom (lastRow, True) xs
drawRow (_, lastRow) =
    \(label, xs) -> do putStr "   "
                       drawRowTop True xs
                       drawRow (False, lastRow) (label, xs)

drawRowLabel :: Char -> IO ()
drawRowLabel label = do putChar ' '
                        putChar label
                        putChar ' '

drawRowTop :: Bool -> Row -> IO ()
drawRowTop _ [] = putStrLn ""
drawRowTop True xs = do putChar '┌'
                        drawRowTop False xs
drawRowTop _ (x:xs) = do if length xs == 0 then
                           putStr "─────┐"
                         else
                           putStr "─────┬"
                         drawRowTop False xs

drawRowMiddle :: Bool -> Row -> IO ()
drawRowMiddle _ [] = putStrLn ""
drawRowMiddle True xs = do putChar '│'
                           drawRowMiddle False xs
drawRowMiddle _ (x:xs) = do putStr "  "
                            putChar x
                            putStr "  │"
                            drawRowMiddle False xs

drawRowBottom :: (Bool, Bool) -> Row -> IO ()
drawRowBottom (_, _) [] = putStrLn ""
drawRowBottom (lastRow, True) xs = do if lastRow then
                                        putChar '└'
                                      else
                                        putChar '├'
                                      drawRowBottom (lastRow, False) xs
drawRowBottom (lastRow, _) (x:xs) = do if lastRow then
                                         putStr "─────"
                                       else
                                         putStr "─────"
                                       putChar (getCellBottomRightChar lastRow xs)
                                       drawRowBottom (lastRow, False) xs

getCellBottomRightChar :: Bool -> Row -> Char
getCellBottomRightChar True xs | length xs == 0 = '┘'
                               | otherwise = '┴'
getCellBottomRightChar False xs | length xs == 0 = '┤'
                                | otherwise = '┼'
