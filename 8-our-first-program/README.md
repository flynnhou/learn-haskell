# 8. Our First Program

### Program.hs

##### Intro

```hs
main :: IO ()
main = putStrLn "hello world"
```

```bash
stack runghc Ipv4.hs
```

- `runghc` is an interpreter, any compile time errors will be produced identically from it. It's similar to run `stack ghc` command
- `::` means "has type"
- `IO` is a type class
- `()` means "unit" representing a value

##### Read file

```hs
main :: IO ()
main = do
    content <- readFile "numbers.txt"
    putStrLn content
```

- `do` creates a do block
- `<-` means "from" or "drawn from"
- try with replacing `putStrLn` with `print` which will yield the '\n'

### FilterNums.hs

### Others

##### Determine `ghc` version

```bash
stack ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 9.0.2
```

##### List `resolvers`

```bash
stack ls snapshots --lts remote | cat
a month ago

Resolver name: lts-18.27
LTS Haskell 18.27 (ghc-8.10.7)

Resolver name: lts-18.26
LTS Haskell 18.26 (ghc-8.10.7)

Resolver name: lts-18.25
LTS Haskell 18.25 (ghc-8.10.7)

4 weeks ago

Resolver name: lts-18.28
LTS Haskell 18.28 (ghc-8.10.7)

3 weeks ago

Resolver name: lts-19.0
LTS Haskell 19.0 (ghc-9.0.2)
...
```

- https://github.com/commercialhaskell/stack/issues/2488
