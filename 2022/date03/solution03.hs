import System.IO
import Data.List

main = do
    input <- readFile "input.txt"
    putStr "1. "
    print $ solution1 $ lines input
    putStr "2. "
    print $ solution2 $ lines input

-- solution1 :: [String] -> a
solution1 = sum . map (getPriority . head . getSame . splitInTwo)

splitInTwo :: String -> (String, String)
splitInTwo items = splitAt (length items `div` 2) items

getSame :: (String, String) -> String
getSame (left, right) = [same | same <- left, x <- right, same == x]

-- Starts at 1
getArrayPosition :: Char -> [Char] -> Int
getArrayPosition del list = length $ foldl shorten "" list
    where shorten acc next
            | del `elem` acc = acc
            | otherwise = next:acc

getPriority :: Char -> Int
getPriority posOf = getArrayPosition posOf (['a'..'z'] ++ ['A'..'Z'])

 ---- Solution 2

solution2  arr = sum . map (getPriority . head . getSameFromThree) $ createTriplets arr

getSameFromThree :: (String, String, String) -> String
getSameFromThree (left, middle, right) =
    [same |
        same <- left, x <- middle, y <- right,
        same == x, x == y]

createTriplets :: [String] -> [(String, String, String)]
createTriplets [] = []
createTriplets (a:b:c:xs) = (a,b,c):(createTriplets xs)
