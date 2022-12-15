import System.IO
import Data.Char
import Data.List (sort, map)
-- import Debug.Trace

import Data.Typeable
-- main = putStrLn "Hello, Haskell! :)"

main = do
    withFile "input.txt" ReadMode findHighest
    withFile "input.txt" ReadMode solution2

findHighest :: Handle -> IO ()
findHighest handle = do
    arrayOfValues <- hGetContents handle
    -- print $ lines arrayOfValues
    putStr "1. "
    print $ getHighest $ toNestedList $ lines arrayOfValues


toNestedList :: [String] -> [[Int]]
toNestedList = foldr foldFunc [[]]
    where
        foldFunc str (x:xs) = if isEmpt str
            then []:x:xs else ((read str):x):xs
        isEmpt str = (length str) == 0

getHighest :: [[Int]] -> Int
getHighest = foldr keepHighest 0
    where
        keepHighest oneElf highest = if sum oneElf > highest
            then sum oneElf
            else highest

solution2 :: Handle -> IO ()
solution2 handle = do
    arrayOfValues <- hGetContents handle
    putStr "2. "
    print . get3Highest . toNestedList . lines $ arrayOfValues

get3Highest :: [[Int]] -> Int
get3Highest = sumTopThree . reverse . sort . sumNested
    where
        sumNested = map sum
        sumTopThree = sum . (take 3)
