# 16. Pattern Matching

## Table of Contents

- Extracting value from instances of algebraic data types
- Pattern matching in function declarations
- Pattern matching in `case..of` expressions
- Exhaustiveness of pattern matches

## Extracting value from instances of algebraic data types

```hs
data Color = RGB Int Int Int deriving Show

main :: IO ()
main = print $ RGB 10 20 30
```

```bash
stack runghc Color.hs 
RGB 10 20 3
```

## Pattern matching in function declarations

```hs
data Color = RGB Int Int Int deriving Show

red :: Color -> Int
red (RGB r _ _) = r

green :: Color -> Int
green (RGB _ g _) = g

blue :: Color -> Int
blue (RGB _ _ b) = b

main :: IO ()
main = do
    let c = RGB 10 20 30
    print $ red c
    print $ green c
    print $ blue c
```

```bash
stack runghc Color.hs 
10
20
30
```

```hs
data Color = RGB Int Int Int deriving Show

data Pixel = Pixel Int Int Int Color

pixelRed :: Pixel -> Int
pixelRed (Pixel _ _ _ (RGB r _ _)) = r

main :: IO ()
main = do
    let p = Pixel 100 200 300 (RGB 10 20 30)
    print $pixelRed p
```

```bash
stack runghc Color.hs 
10
```

```hs
data Color = RGB Int Int Int | CMYK Float Float Float Float deriving Show

colorModel :: Color -> String
colorModel (RGB _ _ _) = "RGB"
colorModel (CMYK _ _ _ _) = "CMYK"

main :: IO ()
main = do
    let c = CMYK 1.0 2.0 3.0 4.0
    putStrLn $ colorModel c
```

```bash
stack runghc Color.hs 
CMYK
```

## Pattern matching in `case..of` expressions

```hs
data Color = RGB Int Int Int | CMYK Float Float Float Float deriving Show

colorModel :: Color -> String
colorModel c =
    case c of RGB _ _ _ -> "RGB"
              CMYK _ _ _ _ -> "CMYK"

main = do
    let c = CMYK 1.0 2.0 3.0 4.0
    putStrLn $ colorModel c
```

```bash
stack runghc ColorCaseOf.hs 
CMYK
```

## Exhaustiveness of pattern matches - What happens if missing a case?

> Runtime error
> 
> If compile with the command `stack ghc ColorCaseOf.hs`, it won't show a compile error

```hs
data Color = RGB Int Int Int | CMYK Float Float Float Float deriving Show

colorModel :: Color -> String
colorModel c =
    case c of RGB _ _ _ -> "RGB"
              CMYK _ _ _ _ -> "CMYK"

main = do
    let c = CMYK 1.0 2.0 3.0 4.0
    putStrLn $ colorModel c
```

```bash
stack runghc ColorCaseOf.hs
ColorCaseOf.hs: ColorCaseOf.hs:5:5-32: Non-exhaustive patterns in case
```

### Compile with `-Wincomplete-patterns`

```bash
stack ghc ColorCaseOf.hs
[1 of 1] Compiling Main             ( ColorCaseOf.hs, ColorCaseOf.o )
Linking ColorCaseOf ...
```

```bash
rm ColorCaseOf.hi ColorCaseOf.o
```

```bash
stack ghc -- -Wincomplete-patterns ColorCaseOf.hs 
[1 of 1] Compiling Main             ( ColorCaseOf.hs, ColorCaseOf.o )

ColorCaseOf.hs:5:5: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: CMYK _ _ _ _
  |
5 |     case c of RGB _ _ _ -> "RGB"
  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Linking ColorCaseOf ...
```

### Lambda Pattern Matching

> Not as powerful when it comes to pattern matching as it only provides one case of matching if the `CMYK` type is added back.

```hs
data Color = RGB Int Int Int deriving Show

red :: Color -> Int
--red (RGB r _ _) = r
red = \(RGB r _ _) -> r

main :: IO ()
main = print $ red (RGB 100 200 300)
```

```bash
stack runghc ColorLambda.hs 
100
```
