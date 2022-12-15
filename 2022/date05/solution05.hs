import System.IO
import Data.List
import Data.List.Split

solution1 raw = splitOn "\r" raw

identifyBoxes :: Int -> Int -> [[Char]] -> [Char]
identifyBoxes row amount stacks = "Temp"

removeBoxes :: Int -> Int -> [[Char]] -> [[Char]]
removeBoxes row amount stacks = ["Temp"]

addBoxes :: Int -> [Char] -> [[Char]] -> [[Char]]
addBoxes row boxes stacks = ["Temp"]

-- parseStacks :: [String] -> [[Char]]
-- parseStacks xs =
--     let rawStacks = splitOn "" xs
--         in [xs]


-- parseCommands :: [String] -> (Int, Int, Int)

main = do
    input <- readFile "example.txt"
    putStr "1. "
    -- print $ solution1 $ lines input
    print $ lines input
    -- putStr "2. "
    -- print $ solution2 $ lines input


