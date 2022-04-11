# 11. Debugging with GHCi

### GHCi Configuration

> Doc: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html#the-ghci-and-haskeline-files

- Global configuration: `~/.ghc/ghci.conf`
- Local configuration: `./.ghci`

##### Example - Catalogue File with Line Numbers in GHCi

- Write a config file (in this case, I simply created a local `.ghci` file)
  ```bash
  :def catn \p -> return $ ":! cat -n \"" ++ p ++ "\""
  ```
- Run the command in `ghci`
  ```bash
  ghci> :catn README.md
  ```

- ghci command `catn` has arg `\p` executing the shell command `cat -n` with the specified file name `p` surrounded by
  double quotes

> From ghci help:
>
> `:def[!] <cmd> <expr>`: define command :<cmd> (later defined command has precedence,
> ::<cmd> is always a builtin command)
> (!: redefine an existing command name)

### Debugging

- Haskell tutorials and online resources don't address much on debugging [or using a debugger]
- GHCi can provide powerful insights into the _runtime_ behavior of programs => see how and when haskell evaluates
  expressions

> This section will use [FilterNums.hs](../8-our-first-program/FilterNums.hs) as the demo.

##### Catalogue the FilterNums.hs

```bash
ghci> :catn ../8-our-first-program/FilterNums.hs
     1  readInts :: String -> [Int]  -- `->`, right arrow, means "maps to" using type signatures and anonymous functions
     2  readInts s = let ws = words s in map read ws  -- `let`, let bindings, introduces a new variable
     3  
     4  minMax :: Ord a => [a] -> Maybe (a, a)  -- `=> ... ->`, double right arrows, implies `a` uses a type constraint to type signature
     5  minMax (h : t) = Just $ foldr  -- `:`, comms, deconstruct the list from head to tail
     6      (\x (min, max) -> (if x < min then x else min, if x > max then x else max))  -- [NEED CLARIFICATION] `\x`, a lambda function
     7      (h, h)
     8      t
     9  minMax _ = Nothing
    10  
    11  main :: IO ()
    12  main = do
    13      content <- readFile "numbers.txt"
    14      let values = readInts content  -- the `let` within do block is its implied scope
    15          count = length values
    16          total = sum values
    17          mean = fromIntegral total / fromIntegral count  -- `fromIntegral` means converting one numeric type to another, must declare it for ints so that float calculation is hence applicable
    18          range = minMax values
    19      print count
    20      print total
    21      print mean
    22      print range
```

##### Load the FilterNums.hs

```bash
ghci> :l ../8-our-first-program/FilterNums.hs 
[1 of 1] Compiling Main             ( ../8-our-first-program/FilterNums.hs, interpreted )
Ok, one module loaded.
```

##### Set Breakpoints

```bash
ghci> :break readInts
Breakpoint 0 activated at ../8-our-first-program/FilterNums.hs:2:34-44
ghci> :break minMax
Breakpoint 1 activated at ../8-our-first-program/FilterNums.hs:(5,18)-(8,5)
Breakpoint 2 activated at ../8-our-first-program/FilterNums.hs:9:12-18
```

> `:break <name>` (`:br`): set a breakpoint on the specified function 
> `minMax` has two definition blocks

##### Run the Main Module to Hit the Breakpoints

```bash
ghci> :main
Stopped in Main.readInts, FilterNums.hs:2:34-44
_result :: [Int] = _
ws :: [String] = _
[FilterNums.hs:2:34-44] ghci>
```

> `_result` is the binding for the as yet the under-evaluation result of the current function
>
> `ws` is its argument in general
>
> `_` (at the right-hand side of each assignment operator) is the placeholder representing the unevaluated _[thunk](https://wiki.haskell.org/Thunk)_

At this point, `_result`, `ws`, variables and arguments can be printed their types and values, or pass them as arguments
to other functions.

```bash
[FilterNums.hs:2:34-44] ghci> :type ws
ws :: [String]
```

##### Print the Value

```bash
[FilterNums.hs:2:34-44] ghci> :sprint ws
ws = _
```

> `:sprint [<name> ...]` (`:sp`): simplified version of :print
>
> `:print [<name> ...]`: show a value without forcing its computation

##### Force to Replace the Thunk with an Actual Value

```bash
[FilterNums.hs:2:34-44] ghci> :force ws
ws = ["32","3920","2","39","99","10","3","109","93","22","91","89",
      "11","92","10","920","2","39","99","10","3","109","93","22","91",
      "89","11","92","10","291","99","10","3","109","93","22","91","89",
      "11","92","10","291","320","44","1"]
```

> `:force <expr>` (`:f`) print `<expr>`, forcing unevaluated parts

After forcing, the following approaches can show the values:

```bash
[FilterNums.hs:2:34-44] ghci> :sp ws
ws = [...]
[FilterNums.hs:2:34-44] ghci> ws
[...]
```

> Note that typing `ws` directly without the print command will always force the thunk.

##### Jump to the Next Breakpoint

```bash
[FilterNums.hs:2:34-44] ghci> :continue
45
7788
173.06666666666666
Stopped in Main.minMax, FilterNums.hs:(5,18)-(8,5)
_result :: Maybe (Int, Int) = _
h :: Int = 32
t :: [Int] = [3920,2,39,99,10,....]
[FilterNums.hs:(5,18)-(8,5)] ghci> 
```

> `:continue`: resume after a breakpoint

Due to Haskell's lazy evaluation, the variable `range` definition (line 18) does not force the expression
until `print` (line 22) **call-by-need** evaluation strategy is encountered.

```bash
[FilterNums.hs:(5,18)-(8,5)] ghci> :sp t
t = [3920,2,39,99,10,3,109,93,22,91,89,11,92,10,920,2,39,99,10,3,
     109,93,22,91,89,11,92,10,291,99,10,3,109,93,22,91,89,11,92,10,291,
     320,44,1]
[FilterNums.hs:(5,18)-(8,5)] ghci> :sp h
h = 32
[FilterNums.hs:(5,18)-(8,5)] ghci> :type t
t :: [Int]
[FilterNums.hs:(5,18)-(8,5)] ghci> :type h
h :: Int
```

##### Get Back to the Debugging - Show Code

```bash
[FilterNums.hs:(5,18)-(8,5)] ghci> :list
4  minMax :: Ord a => [a] -> Maybe (a, a)  -- `=> ... ->`, double right arrows, implies `a` uses a type constraint to type signature
5  minMax (h : t) = Just $ foldr  -- `:`, comms, deconstruct the list from head to tail
6      (\x (min, max) -> (if x < min then x else min, if x > max then x else max))  -- [NEED CLARIFICATION] `\x`, a lambda function
7      (h, h)
8      t
9  minMax _ = Nothing
```

The lines 6-8 are in fact bolded by GHCi.

> `:list` (`:li`): show the source code around current breakpoint

##### Get Back to the Debugging - List Breakpoints

```bash
[FilterNums.hs:(5,18)-(8,5)] ghci> :show breaks
[0] Main FilterNums.hs:2:34-44 enabled
[1] Main FilterNums.hs:(5,18)-(8,5) enabled
[2] Main FilterNums.hs:9:12-18 enabled
```

> `:show` (`:sh`): show the active breakpoints

```bash
[FilterNums.hs:(5,18)-(8,5)] ghci> :show bindings
t :: [Int] = [3920,2,39,99,10,....]
h :: Int = 32
_result :: Maybe (Int, Int) = _
```

##### Leave the Debugging Session

```bash
[FilterNums.hs:(5,18)-(8,5)] ghci> :abandon
ghci>
```

> `:abandon` (`:ab`): at a breakpoint, abandon current computation
