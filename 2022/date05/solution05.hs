import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace

solution1 inp =
    let finalStacks = foldl (\stack command -> executeOne command stack) (parseStacks inp) (parseCommands inp)
        takeFirstBox stack acc = (head stack):acc
        in foldr takeFirstBox "" finalStacks

solution2 inp =
    let finalStacks = foldl (\stack command -> executeOnePartTwo command stack) (parseStacks inp) (parseCommands inp)
        takeFirstBox stack acc = (head stack):acc
        in foldr takeFirstBox "" finalStacks

solution1point5 inp = foldl (\stack command -> executeOne command stack) (parseStacks inp) (parseCommands inp)

identifyBoxes :: Int -> Int -> [[Char]] -> [Char]
identifyBoxes row amount stacks = take amount (stacks!!(row - 1))

removeBoxes :: Int -> Int -> [[Char]] -> [[Char]]
removeBoxes row amount stacks =
    let (front, mod:back) = splitAt (row - 1) stacks
        in front ++ (drop amount mod):back

addBoxes :: Int -> [Char] -> [[Char]] -> [[Char]]
addBoxes row boxes stacks =
    let (front, mod:back) = splitAt (row - 1) stacks
        in front ++ (boxes ++ mod):back

executeOnePartTwo (amount, from, to) setup =
    let boxes = reverse $ identifyBoxes from amount setup
        removeFunc = removeBoxes from amount
        addFunc = addBoxes to boxes
        in addFunc $ removeFunc setup

executeOne (amount, from, to) setup =
    let boxes = identifyBoxes from amount setup
        removeFunc = removeBoxes from amount
        addFunc = addBoxes to boxes
        in addFunc $ removeFunc setup

parseStacks :: [String] -> [[Char]]
parseStacks xs =
    let rawStacks = head $ splitOn [""] xs
        filteredStacks = filter (any isDigit) (transpose rawStacks)
        in map (filter $ not . spaceOrDigit) filteredStacks

spaceOrDigit c = isDigit c || c == ' '

parseCommands :: [String] -> [(Int, Int, Int)]
parseCommands xs =
    let rawCommands = last $ splitOn [""] xs
        justNumbers = map parseCommandLine rawCommands
        in justNumbers

parseCommandLine :: String -> (Int, Int, Int)
parseCommandLine xs =
    let [amount, from, to] = filter (all isDigit) (splitOn " " xs)
        in (read amount, read from, read to)

main = do
    input <- readFile "input.txt"
    putStr "1. "
    print $ solution1 $ lines input
    -- putStr "0.5 "
    -- print $ solution1point5 $ lines input
    -- print $ lines input
    putStr "2. "
    print $ solution2 $ lines input


