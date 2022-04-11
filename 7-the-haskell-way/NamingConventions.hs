module Main (main) where

data MyType = MyDataConstructor String  -- type name should be pascal case;

class MyClass a where
    name :: a -> String

instance MyClass MyType where
    name (MyDataConstructor name) = name

func x = x
func' y = y

main = do
    print $ name (MyDataConstructor "Haskell")
    print $ 100 + 200
    print $ (+) 100 200
