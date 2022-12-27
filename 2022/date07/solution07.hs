import System.IO
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map as Map
import Debug.Trace

-- solution1 cmds = addFileToFiles "/baggins/bill" "1234 lol.txt" ["/no/shit 66"]
extra cmds = getUniquePaths $ parseFiles cmds
-- solution1 cmds = getSubpaths "/a/b/c/d/asda.h" ["/"]
solution1 cmds = sum $ filter (<=100000) (map sizeOfFolder uniqePaths)
    where   files = parseFiles cmds
            uniqePaths = getUniquePaths files
            sizeOfFolder f = getSizeOfFolder f files

-- 70 - (total - x) > 30 => 70 - 30 > total - x => x > total - 40

solution2 cmds = minimum $ filter (>= (totalSize - 40000000)) (map sizeOfFolder uniqePaths)
    where   files = parseFiles cmds
            totalSize = getTotalSize files
            uniqePaths = getUniquePaths files
            sizeOfFolder f = getSizeOfFolder f files

getSizeOfFolder :: String -> [(String, Int)] -> Int
getSizeOfFolder folder files = sum $ map mapToSize (filter filterFiles files)
    where   filterFiles (path, size) = folder `isPrefixOf` path
            mapToSize (path, size) = size

getUniquePaths :: [(String, Int)] -> [String]
getUniquePaths files = nub $ foldl foldUniquePaths ["/"] files
    where foldUniquePaths acc (path, size) = (getSubpaths path []) ++ acc

getSubpaths "/" subPaths = subPaths
getSubpaths path subPaths
    | last path /= '/' = getSubpaths (dropFile path) subPaths
    | otherwise = getSubpaths (backOneUp path) (path:subPaths)
    where   dropFile path = reverse $ dropWhile (/='/') (reverse path)
            backOneUp path = reverse $ dropWhile (/='/') (reverse (init path))


getTotalSize files = foldr f 0 files
    where f (path, size) acc = (+) acc size

parseFiles cmds = sortBy compareTuple (nextCommand "/" cmds [])
    where compareTuple (path1, _) (path2, _) = compare path1 path2

nextCommand :: String -> [String] -> [(String, Int)] -> [(String, Int)]
nextCommand _ [] files = files
nextCommand currPath (cmd:cmdTail) files =
    case parseAction cmd of
        "Change" ->
            if head args == ".."
            then nextCommand (backInPath currPath) cmdTail files
            else nextCommand (addToPath currPath (head args)) cmdTail files
        "Content" -> nextCommand currPath cmdTail (addFileToFiles currPath cmd files)
        "Show" -> nextCommand currPath cmdTail files
        "Directory" -> nextCommand currPath cmdTail files
        where init:action:args = words cmd

addToPath _ "/" = "/"
addToPath currentPath toAdd = currentPath ++ toAdd ++ "/"
backInPath currentPath = reverse $ dropWhile (/='/') (reverse $ init currentPath)

addFileToFiles :: String -> String -> [(String, Int)] -> [(String, Int)]
addFileToFiles currPath line files =
    ((currPath ++ name), (read size)):files
    where [size, name] = words line

parseAction :: String -> String
parseAction cmd
    | action == "cd" = "Change"
    | action == "ls" = "Show"
    | init == "dir" = "Directory"
    | otherwise = "Content"
    where init:action:xs = words cmd

main = do
    input <- readFile "input.txt"
    -- input <- readFile "inputKalaspuff.txt" -- answer=1581595
    putStr "1. "
    print $ solution1 $ lines input
    putStr "2. "
    print $ solution2 $ lines input
    -- putStr "Extra. "
    -- mapM_ print $ extra $ lines input
