# 15. Type Classes

## Table of Contents

- Common type classes
- Type class instances
- Type class instance rules
- Type wrappers
- Writing our own type classes

## Common Type Classes

### `Enum`

```bash
type Enum :: * -> Constraint
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
        -- Defined in ‘GHC.Enum’
instance Enum Word -- Defined in ‘GHC.Enum’
instance Enum Ordering -- Defined in ‘GHC.Enum’
instance Enum Integer -- Defined in ‘GHC.Enum’
instance Enum Int -- Defined in ‘GHC.Enum’
instance Enum Char -- Defined in ‘GHC.Enum’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Enum () -- Defined in ‘GHC.Enum’
instance Enum Float -- Defined in ‘GHC.Float’
instance Enum Double -- Defined in ‘GHC.Float’
```

### `Eq`

```bash
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
        -- Defined in ‘GHC.Classes’
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Eq Word -- Defined in ‘GHC.Classes’
instance Eq Ordering -- Defined in ‘GHC.Classes’
instance Eq Int -- Defined in ‘GHC.Classes’
instance Eq Float -- Defined in ‘GHC.Classes’
instance Eq Double -- Defined in ‘GHC.Classes’
instance Eq Char -- Defined in ‘GHC.Classes’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k, Eq l, Eq m, Eq n) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k, Eq l, Eq m) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k, Eq l) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j, Eq k) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
          Eq j) =>
         Eq (a, b, c, d, e, f, g, h, i, j)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
         Eq (a, b, c, d, e, f, g, h, i)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
         Eq (a, b, c, d, e, f, g, h)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
         Eq (a, b, c, d, e, f, g)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
         Eq (a, b, c, d, e, f)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b) => Eq (a, b) -- Defined in ‘GHC.Classes’
instance Eq () -- Defined in ‘GHC.Classes’
instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
instance (Eq a, Eq b) => Eq (Either a b)
  -- Defined in ‘Data.Either’
instance Eq Integer -- Defined in ‘GHC.Num.Integer’
```

### `Show`

```bash
ghci> :info Show
type Show :: * -> Constraint
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  {-# MINIMAL showsPrec | show #-}
        -- Defined in ‘GHC.Show’
instance [safe] Show Color -- Defined at ColorRGBCMYK.hs:3:70
instance (Show a, Show b) => Show (Either a b)
  -- Defined in ‘Data.Either’
instance Show a => Show [a] -- Defined in ‘GHC.Show’
instance Show Word -- Defined in ‘GHC.Show’
instance Show a => Show (Solo a) -- Defined in ‘GHC.Show’
instance Show GHC.Types.RuntimeRep -- Defined in ‘GHC.Show’
instance Show Ordering -- Defined in ‘GHC.Show’
instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
instance Show Integer -- Defined in ‘GHC.Show’
instance Show Int -- Defined in ‘GHC.Show’
instance Show Char -- Defined in ‘GHC.Show’
instance Show Bool -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l, Show m) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l, m)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k, Show l) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j, Show k) =>
         Show (a, b, c, d, e, f, g, h, i, j, k)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i, Show j) =>
         Show (a, b, c, d, e, f, g, h, i, j)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h, Show i) =>
         Show (a, b, c, d, e, f, g, h, i)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
          Show h) =>
         Show (a, b, c, d, e, f, g, h)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f,
          Show g) =>
         Show (a, b, c, d, e, f, g)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e, Show f) =>
         Show (a, b, c, d, e, f)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d, Show e) =>
         Show (a, b, c, d, e)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b, Show c) => Show (a, b, c)
  -- Defined in ‘GHC.Show’
instance (Show a, Show b) => Show (a, b) -- Defined in ‘GHC.Show’
instance Show () -- Defined in ‘GHC.Show’
instance Show Float -- Defined in ‘GHC.Float’
instance Show Double -- Defined in ‘GHC.Float’
```


### `Num`

```bash
:info Num
type Num :: * -> Constraint
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
        -- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

## Type Class Instances

### Quaternion

```hs
data Quaternion = Q
    { qR :: Double
    , qI :: Double
    , qJ :: Double
    , qK :: Double
    }

main :: IO ()
main = print $ Q 1 2 3 4
```

```bash
stack runghc Quaternion.hs 

Quaternion.hs:10:8: error:
    • No instance for (Show Quaternion) arising from a use of ‘print’
    • In the first argument of ‘($)’, namely ‘print’
      In the expression: print $ Q 1 2 3 4
      In an equation for ‘main’: main = print $ Q 1 2 3 4
   |
10 | main = print $ Q 1 2 3 4
   |        ^^^^^
```

```hs
...

instance Show Quaternion where
    show q = "(" ++
        show (qR q) ++ " + " ++
        show (qI q) ++ "i + " ++
        show (qJ q) ++ "j + " ++
        show (qK q) ++ "k)"

...
```

```bash
stack runghc Quaternion.hs 
(1.0 + 2.0i + 3.0j + 4.0k)
```

```hs
data Quaternion = Q
    { qR :: Double
    , qI :: Double
    , qJ :: Double
    , qK :: Double
    } deriving Show
```

```bash
stack runghc Quaternion.hs 
Q {qR = 1.0, qI = 2.0, qJ = 3.0, qK = 4.0}
```

#### Implement `Num` Instance by Defining the MINIMAL

```hs
instance Num Quaternion where
    q0 + q1 = Q (qR q0 + qR q1) (qI q0 + qI q1) (qJ q0 + qJ q1) (qK q0 + qK q1)
    q0 * q1 = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined
```

```bash
stack runghc Quaternion.hs
Q {qR = 11.0, qI = 22.0, qJ = 33.0, qK = 44.0}
```

## Type Class Instance Rules

- Don't define instance for other people's data types
- Language has no mechanism for choosing an instance
- There are good reasons for this
- There are workarounds

## Type Wrappers

### How to Wrap Type?

> This will create a lot of wrap, unwrap overheads.

```hs
data Quaternion = Q { qR :: Double, qI :: Double, qJ :: Double, qK :: Double }

data PrettyQuaternion = PrettyQuaternion { unPrettyQuaternion :: Quaternion }
instance Show PrettyQuaternion where
    show q = let p = unPrettyQuaternion q in "(" ++
        show (qR p) ++ " + " ++
        show (qI p) ++ "i + " ++
        show (qJ p) ++ "j + " ++
        show (qK p) ++ "k)"

data UglyQuaternion = UglyQuaternion { unUglyQuaternion :: Quaternion }
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
```

```bash
stack runghc TypeWrapping.hs 
(1.0 + 2.0i + 3.0j + 4.0k)
1.0,2.0,3.0,4.0)
```

> Replace `data` with `newtype` => minimal overhead

```hs
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
```

```bash
stack runghc TypeWrapping.hs 
(1.0 + 2.0i + 3.0j + 4.0k)
1.0,2.0,3.0,4.0)
```

## Writing Our Own Type Classes

```hs
class Frobber a where
    frob :: a -> (String, Integer)  -- abstract method

data A = A { aValue :: Int }
instance Frobber A where
    frob a = let value = aValue a in (show value, toInteger value)

data B = B { bValue :: Integer }
instance Frobber B where
    frob b = let value = bValue b in (show value, toInteger value)

data C = C { cValue :: Double }
instance Frobber C where
    frob c = let value = cValue c in (show value, round value)

printFrobResult :: Frobber a => a -> IO ()
printFrobResult = print . frob  -- function composition

main :: IO ()
main = do
    printFrobResult (A 100)
    printFrobResult (B (2 ^ 70))
    printFrobResult (C 3.141)
```

```bash
stack runghc TypeClasses.hs 
("100",100)
("1180591620717411303424",1180591620717411303424)
("3.141",3)
```
