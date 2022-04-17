import Data.List

formatList :: String -> String -> String -> [String] -> String
formatList start end separator xs = start ++ (intercalate separator (map show xs)) ++ end

main :: IO ()
main = putStrLn $ formatList "<list>" "</list>" "|" ["first", "second", "third", "fourth"]
