readInts :: String -> [Int]  -- `->`, right arrow, means "maps to" using type signatures and anonymous functions
readInts s = let ws = words s in map read ws  -- `let`, let bindings, introduces a new variable

minMax :: Ord a => [a] -> Maybe (a, a)  -- `=> ... ->`, double right arrows, implies `a` uses a type constraint to type signature
minMax (h : t) = Just $ foldr  -- `:`, comms, deconstruct the list from head to tail
    (\x (min, max) -> (if x < min then x else min, if x > max then x else max))  -- [NEED CLARIFICATION] `\x`, a lambda function
    (h, h)
    t
minMax _ = Nothing

main :: IO ()
main = do
    content <- readFile "numbers.txt"
    let values = readInts content  -- the `let` within do block is its implied scope
        count = length values
        total = sum values
        mean = fromIntegral total / fromIntegral count  -- `fromIntegral` means converting one numeric type to another, must declare it for ints so that float calculation is hence applicable
        range = minMax values
    print count
    print total
    print mean
    print range
