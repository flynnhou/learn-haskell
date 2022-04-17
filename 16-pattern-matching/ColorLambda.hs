data Color = RGB Int Int Int deriving Show

red :: Color -> Int
--red (RGB r _ _) = r
red = \(RGB r _ _) -> r

main :: IO ()
main = print $ red (RGB 100 200 300)
