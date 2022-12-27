import System.IO
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map as Map
import Debug.Trace

solution1 cmds = sum $ filter (<100000) (getAllSizesForP1 (parseFileStructure cmds))

getAllSizes :: [[String]] -> [(String, Int)]
getAllSizes fs = map mapFunction fs
    where
        mapFunction dirArray =
            let dir = head dirArray
            in (dir, (calculateDirectorySize dir fs))

getAllSizesForP1 :: [[String]] -> [Int]
getAllSizesForP1 fs = map mapFunction fs
    where
        mapFunction dirArray =
            let dir = head dirArray
            in (calculateDirectorySize dir fs)

calculateDirectorySize :: String -> [[String]] -> Int
calculateDirectorySize dir fs =
    let sum = foldl foldCalc 0 allContent
        allContent = tail $ getDir dir fs
        isFile row = all isDigit (head $ words row)
        isDir row = "dir" == (head $ words row)
        foldCalc acc fileOrDir
            | isFile fileOrDir = acc + (read $ head $ words fileOrDir)
            | isDir fileOrDir = acc + (calculateDirectorySize (last $ words fileOrDir) fs)
            | otherwise = acc
        in sum

getDir :: String -> [[String]] -> [String]
getDir dir fs = head $ filter (isDir dir) fs

parseFileStructure :: [String] -> [[String]]
parseFileStructure commandList = buildNextCommand "/" commandList []

findParent :: String -> [[String]] -> String
findParent currDir fs = head $ head $ filter ("dir " ++ currDir `elem`) fs

buildNextCommand :: String -> [String] -> [[String]] -> [[String]]
buildNextCommand currDir [] fs = fs
buildNextCommand currDir (cmd:cmdTail) fs =
    case parseAction cmd of
        "Change" ->
            if head args == ".."
            then buildNextCommand (findParent currDir fs) cmdTail fs
            else buildNextCommand (head args) cmdTail fs
        "Show" -> buildNextCommand currDir cmdTail fs
        "Content" -> buildNextCommand currDir cmdTail (addContentToFs currDir cmd fs)
        where init:action:args = words cmd

parseAction :: String -> String
parseAction cmd
    | action == "cd" = "Change"
    | action == "ls" = "Show"
    | otherwise = "Content"
    where init:action:xs = words cmd

addContentToFs :: String -> String -> [[String]] -> [[String]]
addContentToFs currDir content fs =
    case findIndex (isDir currDir) fs of
        Just index -> updateIndex index fs (++ [content])
        Nothing -> [currDir, content]:fs

-- Haskoin.Util
updateIndex i xs f
    | i < 0 || i >= length xs = xs
    | otherwise = l ++ (f h : r)
  where
    (l, h : r) = splitAt i xs
----

isDir :: String -> [String] -> Bool
isDir name dir = (name == head dir)

main = do
    input <- readFile "input.txt"
    putStr "1. "
    print $ solution1 $ lines input
    -- putStr "2. "
    -- print $ solution2 input
    -- putStr "Extra. "
    -- print $ extra $ lines input
