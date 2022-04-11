# 12. Values and Expressions

## Table of Contents

- Values of Primitive Types
- Functions as Values
- Function Application
- Function Composition
- Anonymous Functions
- Infix Functions, Sections, and Partial Application

## Primitive Types

- **Char**: Unicode character
- **Integer**: Mathematical integer, arbitrary precision
- **Int**: Machine integer, e.g., int32, int64 signed values
- **Float, Double**: Single or double precision floating point numbers
- Almost everything else is defined in terms of these (including the string type)

### GHCi Playground

##### Int vs Integer

```bash
ghci> a :: Int; a = 1234
ghci> :sp a
a = _
ghci> :t a
a :: Int
ghci> a
1234
ghci> :sp a
a = 1234
ghci> 
ghci> b :: Integer; b = 2 ^ 70
ghci> :sp b
b = _
ghci> :t b
b :: Integer
ghci> b
1180591620717411303424
ghci> :sp b
b = 1180591620717411303424
```

Haskell does not allow implicit conversion between numeric types by design:

```bash
ghci> a :: Int; a = 2 ^ 70
ghci> a
0
```

## Functions

### GHCi Playground

##### Import a Module to Access a Function

```bash
ghci> import Data.List
ghci> :type intercalate
intercalate :: [a] -> [[a]] -> [a]
ghci>
```

##### Use `intercalate`

```bash
ghci> intercalate ":" ["/path/to/dir0", "/path/to/dir1"]
"/path/to/dir0:/path/to/dir1"
ghci>
```

##### Create a Function to PrettyPrint a List

###### Declaration

```bash
ghci> formatList s e sep xs = s ++ (intercalate sep (map show xs)) ++ e
```

```bash
ghci> :t formatList
formatList :: Show a => [Char] -> [Char] -> [Char] -> [a] -> [Char]
```

###### Invocation

```bash
ghci> formatList "(" ")" ", " [1, 2, 3, 4]
"(1, 2, 3, 4)"
```

###### Breakdown - `map show <list>`

```bash
ghci> map show [1, 2, 3, 4]
["1","2","3","4"]
```

###### Breakdown - `intercalate <sep> (...)`

```bash
ghci> intercalate ", " (map show [1, 2, 3, 4])
"1, 2, 3, 4"
```

##### Create `sauare` and `squareSum` Functions

> Whitespace, precedence,

###### Declaration

```bash
ghci> square x = x ^ 2
ghci> squareSum x y = square x + square y
ghci> squareSum 3 4
25
ghci>
ghci>
ghci> :t square
square :: Num a => a -> a
ghci> :t squareSum
squareSum :: Num a => a -> a -> a
```

##### ???

###### Declaration

```bash
ghci> f = let s = "hello world" in putStrLn $ "(" ++ s ++ ")"
```

###### Invocation
```bash
ghci> f
(hello world)
```

###### Equivalent Declaration

```bash
ghci> f = let s = "hello world" in putStrLn ("(" ++ s ++ ")")
ghci> f
(hello world)
```


## Functions Composition

### Dot Annotation

##### f(g(x)) <--> (f.g)(x)

```bash
ghci> doubleIt x = x * 2
ghci> :t doubleIt
doubleIt :: Num a => a -> a
ghci> 
ghci> addTen x = x + 10
ghci> :t addTen
addTen :: Num a => a -> a
ghci> 
ghci> addTen (doubleIt 5)
20
ghci> (addTen . doubleIt) 5
20
ghci>
```

##### f(g(h(x))) <--> (f.g.h)(x)

```bash
ghci> show (addTen (doubleIt 5))
"20"
ghci> (show . addTen . doubleIt) 5
"20"
```

##### Alias

```bash
ghci> f = show . addTen . doubleIt
ghci> :t f
f :: (Show b, Num b) => b -> String
ghci> map f [10, 11, 12, 13, 14]
["30","32","34","36","38"]
```

##### Lambda Function

```bash
ghci> \x -> x + 1
```

> pronounces as "lambda x maps to x plus one"

###### Aliases

```bash
ghci> -- \x y -> x + y
ghci> -- \x -> \y -> x + y
ghci> -- \x -> (\y -> x + y)
```

##### Create `parenthesizeWords` Function

###### Approach Without Using Lambda Function
```bash
ghci> :{
ghci| parenthesizeWords s = unwords $ map parenthesizeWord (words s)
ghci|     where parenthesizeWord s = "(" ++ s ++ ")"
ghci| :}
ghci> :t parenthesizeWords
parenthesizeWords :: String -> String
ghci> 
ghci> parenthesizeWords "We love Haskell"
"(We) (love) (Haskell)"
```

```bash
ghci> :t unwords
unwords :: [String] -> String
ghci> :t words
words :: String -> [String]
```

###### Approach Using Lambda Function

```bash
ghci> parenthesizeWords s = unwords $ map (\s -> "(" ++ s ++ ")") (words s)
ghci> parenthesizeWords "We love Haskell"
"(We) (love) (Haskell)"
```

## Infix Functions, Sections, and Partial Application

### `parenthesizeWord` Alternatives

```bash
ghci> parenthesizeWord = \s -> "(" ++ s ++ ")"
ghci> parenthesizeWord "We"
"(We)"
ghci> :t parenthesizeWord
parenthesizeWord :: [Char] -> [Char]
ghci> 
ghci> parenthesizeWord s = "(" ++ s ++ ")"
ghci> parenthesizeWord "love"
"(love)"
ghci> :t parenthesizeWord
parenthesizeWord :: [Char] -> [Char]
ghci>
ghci> parenthesizeWord = ("(" ++) . (++ ")")
ghci> parenthesizeWord "functions"
"(functions)"
ghci> :t parenthesizeWord
parenthesizeWord :: [Char] -> [Char]
ghci> :t ("(" ++)             -- "section": prepend string left parenthesis to an argument, aka "partial application"
("(" ++) :: [Char] -> [Char]
ghci> :t (++ ")")             -- "section": append string right parenthesis to an argument, aka "partial application"
(++ ")") :: [Char] -> [Char]
```

## References

- [Lambda abstraction](https://wiki.haskell.org/Lambda_abstraction)
- [Section of an infix operator](https://wiki.haskell.org/Section_of_an_infix_operator)
- [Partial application](https://wiki.haskell.org/Partial_application)
- [Infix operator](https://wiki.haskell.org/Infix_operator)
