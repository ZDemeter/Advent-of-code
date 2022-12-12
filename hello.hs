import System.IO
-- main = putStrLn "Hello, Haskell! :)"

main = do
    withFile "input_1.txt" ReadMode (\handle -> do
        content <- hGetContents handle
        putStr content)

-- test :: Handle -> IO String
-- test handle = do
--     content <- hGetContents handle
--     hClose handle
--     return content
