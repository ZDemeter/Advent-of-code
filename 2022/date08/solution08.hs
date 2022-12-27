import System.IO
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map as Map
import Debug.Trace


solution2 = getBestScore . mapToInts

getScore :: [[Int]] -> (Int, Int) -> Int
getScore area (x,y) =
    let (left, target:right) = splitAt x (area!!y)
        (top, transTarget:bottom) = splitAt y $ (transpose area)!!x
        flipLeft = reverse left
        flipTop = reverse top
        [t1,t2,t3,t4] = map (countVisibleTrees target) [right, bottom, flipLeft, flipTop]
        -- in trace (show [right,bottom,flipLeft,flipTop,[target]]) (t1*t2*t3*t4)
        in (t1*t2*t3*t4)

getBestScore :: [[Int]] -> Int
getBestScore area =
    let height = length area
        width = length $ transpose area
        allPoints = [(x,y) | x <- [0..(width-1)], y <- [0..(height-1)]]
        in maximum $ map ((getScore area)) allPoints

countVisibleTrees :: Int -> [Int] -> Int
countVisibleTrees _ [] = 0
countVisibleTrees targetHeight row =
    let visibleRow = takeWhile (<targetHeight) row
        -- score = (length (takeWhile (<targetHeight) row)) + 1
        in if visibleRow == row then length row else (length visibleRow) + 1

solution1 = countVisible . mapToInts

countVisible :: [[Int]] -> Int
countVisible area =
    let height = length area
        width = length $ transpose area
        allPoints = [(x,y) | x <- [0..(width-1)], y <- [0..(height-1)]]
        in sum $ map (fromEnum . (isVisible area)) allPoints

mapToInts :: [String] -> [[Int]]
mapToInts area = map mapRowToInt area
    where mapRowToInt row = map digitToInt row

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible area (x,y) =
    let (left, target:right) = splitAt y (area!!x)
        (top, transTarget:bottom) = splitAt x $ (transpose area)!!y
        in any (not . any (>=target)) [left, right, top, bottom]

-- isVisibleInRow :: (Int, Int) -> [Int] -> Bool
-- isVisibleInRow (x, y) row =
--     let (left, point : right) = splitAt x row



main = do
    input <- readFile "input.txt"
    putStr "1. "
    print $ solution1 $ lines input
    putStr "2. "
    print $ solution2 $ lines input
    -- putStr "Extra. "
    -- mapM_ print $ extra $ lines input