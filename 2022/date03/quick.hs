import Data.List
-- import Data.List.Split

-- main = print [x | x <- ['a','b','x'], y <- ['b','c','a'], x == y]

main = print $ foldl (\acc x -> if 'E' `elem` acc then acc else x:acc ) "" "ABCDEFGH"

