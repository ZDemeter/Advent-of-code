import System.IO
import Data.List
import Data.Char
import Debug.Trace

solution1 x = length $ nextMove (0, 0) (0, 0) 'U' 0 x []
solution2 x = length $ nextMovePart2 (replicate 10 (0,0)) 'U' 0 x []
-- solution1 x = paintSnake [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9),(10,10)]

paintPos (xH,yH) (xT,yT) =
    let canvas = replicate 30 (replicate 30 '.')
        offset = 15
        withH = putIn 'H' (xH+offset,yH+offset) canvas
        withT = putIn 'T' (xT+offset,yT+offset) withH
        in withT

paintSnake snake =
    let canvas = replicate 30 (replicate 30 '.')
        offset = 15
        painted = paintNext canvas 9 snake
        in reverse painted

paintNext canvas (-1) _ = canvas
paintNext canvas order snake =
    let (x,y) = snake!!order
        vis = intToDigit order
        newCanvas = putIn vis (x+15, y+15) canvas
        in paintNext newCanvas (order-1) snake

putIn ch (x,y) xs = putInRow (putInRow ch x (xs!!y)) y xs

putInRow ch pos xs = l ++ ( ch : r )
    where (l, _ : r) = splitAt pos xs

norm :: Int -> Int
norm 0 = 0
norm num = if num > 0 then 1 else (-1)

moveJoint (xPos,yPos) (xNew,yNew) (xOld,yOld) =
    let moveIt = doesTailMove (xNew, yNew) (xPos, yPos)
        xMove = norm (xNew - xPos)
        yMove = norm (yNew - yPos)
        in if moveIt
            then (xPos + xMove, yPos + yMove)
            else (xPos, yPos)

nextMovePart2 :: [(Int,Int)] -> Char -> Int -> [String] -> [(Int, Int)] -> [(Int, Int)]
nextMovePart2 _ _ 0 [] visited = nub visited
nextMovePart2 snake direction 0 (cmd:rest) visited =
    nextMovePart2 snake (head newDir) (read moves) rest visited
    -- where [newDir, moves] = words (trace (show $ paintSnake snake) cmd)
    where [newDir, moves] = words cmd
nextMovePart2 snake direction movesLeft cmds visited =
    let newPos0 = moveHead (snake!!0) direction
        newPos1 = moveJoint (snake!!1) newPos0 (snake!!0)
        newPos2 = moveJoint (snake!!2) newPos1 (snake!!1)
        newPos3 = moveJoint (snake!!3) newPos2 (snake!!2)
        newPos4 = moveJoint (snake!!4) newPos3 (snake!!3)
        newPos5 = moveJoint (snake!!5) newPos4 (snake!!4)
        newPos6 = moveJoint (snake!!6) newPos5 (snake!!5)
        newPos7 = moveJoint (snake!!7) newPos6 (snake!!6)
        newPos8 = moveJoint (snake!!8) newPos7 (snake!!7)
        newPos9 = moveJoint (snake!!9) newPos8 (snake!!8)
        newVisited = (newPos9:visited)
        -- newSnake = trace (show [newPos0, newPos9]) ([
        newSnake =[
            newPos0,
            newPos1,
            newPos2,
            newPos3,
            newPos4,
            newPos5,
            newPos6,
            newPos7,
            newPos8,
            newPos9]
        in nextMovePart2 newSnake direction (movesLeft - 1) cmds newVisited

nextMove :: (Int,Int) -> (Int,Int) -> Char -> Int -> [String] -> [(Int, Int)] -> [(Int, Int)]
nextMove _ _ _ 0 [] visited = nub visited
nextMove hPos tPos direction 0 (cmd:rest) visited =
    let [newDir, moves] = words cmd
        in nextMove hPos tPos (head newDir) (read moves) (trace (show ("Doing " ++ moves ++ " " ++ newDir)) rest) visited
nextMove hPos tPos direction movesLeft cmds visited =
    let newHpos = moveHead hPos direction
        newTpos = if (doesTailMove newHpos tPos)
            then hPos -- it always does to the previous position of knot ahead
            else tPos
        newVisited = (newTpos:visited)
        in nextMove newHpos newTpos direction (movesLeft - 1) cmds newVisited

getBehind :: (Int, Int) -> Char -> (Int, Int)
getBehind (x,y) direction =
    case direction of
        'U' -> (x, y - 1)
        'R' -> (x - 1, y)
        'D' -> (x, y + 1)
        'L' -> (x + 1, y)

moveHead :: (Int, Int) -> Char -> (Int, Int)
moveHead (oldX, oldY) direction =
    case direction of
        'U' -> (oldX, oldY + 1)
        'R' -> (oldX + 1, oldY)
        'D' -> (oldX, oldY - 1)
        'L' -> (oldX - 1, oldY)

doesTailMove :: (Int, Int) -> (Int, Int) -> Bool
doesTailMove (xH,yH) (xT, yT) =
    let [x1,y1, x2,y2] = map fromIntegral [xH,yH,xT,yT]
        in (sqrt (((x1 - x2) ** 2) + ((y1 - y2) ** 2))) > (sqrt 2)

main = do
    input <- readFile "input.txt"
    putStr "1. "
    print $ solution1 $ lines input
    putStr "2. "
    print $ solution2 $ lines input


