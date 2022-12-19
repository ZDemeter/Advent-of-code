import System.IO
import Data.List
import Data.List.Split
import Data.Char

findFourDiff :: Int -> String -> Int
findFourDiff position (a:b:c:d:[])
    | (length $ nub (a:b:c:d:[])) == 4 = position
    | otherwise = -1
findFourDiff position (a:b:c:d:rest)
    | (length $ nub (a:b:c:d:[])) == 4 = position
    | otherwise = findFourDiff (position + 1) (b:c:d:rest)

solution1 = (findFourDiff 4)

-------

findMessageMarker :: Int -> String -> Int
findMessageMarker position packet =
    let checkMarker = take 14 packet
        isMarker = (length $ nub checkMarker) == 14
        in if isMarker then position else findMessageMarker (position + 1) (tail packet)

solution2 = (findMessageMarker 14)

main = do
    input <- readFile "input.txt"
    putStr "1. "
    print $ solution1 input
    putStr "2. "
    print $ solution2 input
