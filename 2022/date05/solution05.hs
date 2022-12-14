import System.IO
import Data.List
import Data.List.Split

main = do
    input <- readFile "example.txt"
    putStr "1. "
    print $ solution1 $ lines input
    -- putStr "2. "
    -- print $ solution2 $ lines input


