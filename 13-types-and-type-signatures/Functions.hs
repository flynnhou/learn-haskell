f0 :: String -> Int
f0 = length

f1 :: String -> (String, Int)
f1 x = (x, length x)

f2 :: [String] -> [(String, Int)]
f2 = map f1

main :: IO ()
main = do
    print $ f0 "hello"
    print $ f1 "hello"
    print $ f2 ["Hello", "Goodbye"]
