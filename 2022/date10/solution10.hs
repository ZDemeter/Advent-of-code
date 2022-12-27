import System.IO
import Data.List
import Data.Char
import Debug.Trace

solution2 program = splitForScreen [] (draw $ createCycleValues $ parse program)

splitForScreen :: [String] -> String -> [String]
splitForScreen twoDimList [] = twoDimList
splitForScreen twoDimList oneDimString =
    let (left, right) = splitAt 40 oneDimString
        newTwoDim = twoDimList ++ [left]
        in splitForScreen newTwoDim right

draw :: [Int] -> [Char]
draw cycleValues =
    let indexedValues = zip (cycle [0..39]) cycleValues
        whatToDraw (i, spriteX) = if (i `elem` (getSprite spriteX)) then '#' else '.'
        in map whatToDraw indexedValues

createCycleValues :: [Int] -> [Int]
createCycleValues cycles = foldl calcCycle [1] cycles
    where calcCycle acc cycle = acc ++ [(cycle + (last acc))]

getSprite x = [x-1,x,x+1]

--------------------- p2 ^

solution1 program = sum $ calcStrength $ getXList $ parse program
extra p = map words p

calcStrength xList = zipWith (*) xList getPoints
-- calcValue

getPoints = [20, 60, 100, 140, 180, 220]

getXList :: [Int] -> [Int]
getXList cmdValues = map calcX getPoints
    where calcX cycle = (1+) $ sum $ take (cycle-1) cmdValues

parse :: [String] -> [Int]
parse program = foldr simplify [] (map words program)
    where
        simplify cmd acc
            | head cmd == "addx" = 0:(read $ cmd!!1):acc
            | otherwise = 0:acc

main = do
    input <- readFile "input.txt"
    putStr "1. "
    print $ solution1 $ lines input
    print "2. "
    mapM_ print $ solution2 $ lines input
    -- putStr "Extra. "
    -- print $ extra $ lines input