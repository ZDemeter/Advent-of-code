import System.IO
import Data.List

data Hand = Rock | Paper | Scissors deriving (Eq, Ord, Read, Show, Enum)

beats :: Hand -> Hand -> Bool
beats Rock otherHand = otherHand == Scissors
beats h1 h2= pred h1 == h2

winTo :: Hand -> Hand
winTo Scissors = Rock
winTo h = succ h

loseTo :: Hand -> Hand
loseTo Rock = Scissors
loseTo h = pred h

main = do
    input <- readFile "input.txt"
    putStr "1. "
    solution1 $ lines input
    putStr "2. "
    solution2 $ lines input


solution1 :: [String] -> IO ()
-- solution1 = print . sum . map (evaluatePoints . parseHand)
solution1 x = print x

parseHand :: String -> (Hand, Hand)
parseHand rawGame = let
    [you, me] = map getHand (words rawGame);
    getHand (option:_)
        | option `elem` ['A','X'] = Rock
        | option `elem` ['B','Y'] = Paper
        | option `elem` ['C','Z'] = Scissors
        in (you, me)

evaluatePoints :: (Hand, Hand) -> Int
evaluatePoints (you, me)
    | me `beats` you = 6 + getHandValue me
    | you `beats` me = getHandValue me
    | otherwise = 3 + getHandValue me

getHandValue :: Hand -> Int
getHandValue myHand
    | myHand == Rock = 1
    | myHand == Paper = 2
    | myHand == Scissors = 3

solution2 :: [String] -> IO ()
solution2 = print . sum . map (evaluatePoints . parseHand2)

parseHand2 :: String -> (Hand, Hand)
parseHand2 rawGame = let
    you = getHand . head . words $ rawGame
    getHand (option:_)
        | option `elem` ['A'] = Rock
        | option `elem` ['B'] = Paper
        | option `elem` ['C'] = Scissors
    me = getMyHand . last . words $ rawGame
    getMyHand winOrLose
        | winOrLose == "X" = loseTo you
        | winOrLose == "Y" = you
        | winOrLose == "Z" = winTo you
        in (you, me)