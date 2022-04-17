c0 :: Char
c0 = 'X'

c1 :: Char
c1 = '\0088'

c2 :: Char
c2 = '\x0058'

c3 :: Char
c3 = '\o0130'

main :: IO ()
main = do
    putChar c0 >> putChar '\n'
    putChar c1 >> putChar '\n'
    putChar c2 >> putChar '\n'
    putChar c3 >> putChar '\n'
