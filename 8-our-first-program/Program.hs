-- main :: IO ()

-- intro
-- main = putStrLn "hello world"

-- read file
main = do
    content <- readFile "numbers.txt"
    putStrLn content
