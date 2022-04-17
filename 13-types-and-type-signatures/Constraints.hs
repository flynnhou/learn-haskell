myFold :: (a -> b -> b) -> b -> [a] -> b
myFold _ b [] = b
myFold f b (a : as) = myFold f (f a b) as

--mySum :: Num a => [a] -> a  -- the definition gotten from `:type (+)`
--mySum :: _
mySum :: [Integer] -> Integer
mySum = myFold (+) 0

main :: IO ()
--main = print $ myFold (+) 100 [10, 20, 30]
main = print $ mySum [10, 20, 30]
