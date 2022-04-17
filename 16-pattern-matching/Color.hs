--data Color = RGB Int Int Int deriving Show

--red :: Color -> Int
--red (RGB r _ _) = r
--
--green :: Color -> Int
--green (RGB _ g _) = g
--
--blue :: Color -> Int
--blue (RGB _ _ b) = b

--data Pixel = Pixel Int Int Int Color

--pixelRed :: Pixel -> Int
--pixelRed (Pixel _ _ _ (RGB r _ _)) = r

data Color = RGB Int Int Int | CMYK Float Float Float Float deriving Show

colorModel :: Color -> String
colorModel (RGB _ _ _) = "RGB"
colorModel (CMYK _ _ _ _) = "CMYK"

main :: IO ()
--main = print $ RGB 10 20 30
--main = do
--    let c = RGB 10 20 30
--    print $ red c
--    print $ green c
--    print $ blue c
--main = do
--    let p = Pixel 100 200 300 (RGB 10 20 30)
--    print $pixelRed p
main = do
    let c = CMYK 1.0 2.0 3.0 4.0
    putStrLn $ colorModel c
