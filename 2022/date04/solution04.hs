import System.IO
import Data.List
import Data.List.Split

main = do
    input <- readFile "input.txt"
    putStr "1. "
    print $ solution1 $ lines input
    putStr "2. "
    print $ solution2 $ lines input

solution1 dt = length $ filter containsBackOrForth (map parseLine dt)

parseLine :: String -> ((Int, Int),(Int, Int))
parseLine str =
    let [sectionOne, sectionTwo] = splitOn "," str
        [lowOne, highOne] = map read (splitOn "-" sectionOne)
        [lowTwo, highTwo] = map read (splitOn "-" sectionTwo)
        in ((lowOne, highOne), (lowTwo, highTwo))

containsBackOrForth :: ((Int, Int),(Int, Int)) -> Bool
containsBackOrForth (t1, t2) = fullyContains t1 t2 || fullyContains t2 t1

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
fullyContains (low, high) (testLow, testHigh) = testLow >= low && testHigh <= high

---- Solution 2

overlaps :: ((Int, Int),(Int, Int)) -> Bool
overlaps ((low1,high1),(low2,high2)) = not (low1 > high2 || high1 < low2)

solution2 dt = length $ filter overlaps (map parseLine dt)
