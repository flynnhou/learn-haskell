data Quaternion = Q { qR :: Double, qI :: Double, qJ :: Double, qK :: Double }

newtype PrettyQuaternion = PrettyQuaternion { unPrettyQuaternion :: Quaternion }
instance Show PrettyQuaternion where
    show q = let p = unPrettyQuaternion q in "(" ++
        show (qR p) ++ " + " ++
        show (qI p) ++ "i + " ++
        show (qJ p) ++ "j + " ++
        show (qK p) ++ "k)"

newtype UglyQuaternion = UglyQuaternion { unUglyQuaternion :: Quaternion }
instance Show UglyQuaternion where
    show q = let p = unUglyQuaternion q in
        show (qR p) ++ "," ++
        show (qI p) ++ "," ++
        show (qJ p) ++ "," ++
        show (qK p) ++ ")"

main :: IO ()
main = do
    print $ PrettyQuaternion (Q 1 2 3 4)
    print $ UglyQuaternion (Q 1 2 3 4)
