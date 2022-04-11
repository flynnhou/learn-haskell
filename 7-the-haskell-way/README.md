# 7. The Haskell Way

### Exercises

###### add brackets

```hs
addBrackets s = "[" ++ s ++ "]"
result = map addBrackets ["one", "two", "three"]
main = print result
```

> `++` is the append operator

- Run the haskell on the fly without compiling it
  ```bash
  stack runghc AddBrackets.hs
  ```

###### factorial

```hs
factorial n = if n < 2 then 1 else n * factorial (n - 1)
main = print $ factorial 5
```

### Pure Functions Everywhere

- **All functions are pure in Haskell**
- Pure functions are not a requirement for an functional programming language
- This is the core to generating efficient code

### Non-Strict Evaluation

- OOP languages are mostly strict evaluation
- Haskell is non-strict evaluation
- _Call-by-need_
- **Lazy evaluation**: Function arguments are not evaluated unless they're actually used. They are evaluated on demand and the results will be memoized for future use.

###### control structure

```hs
myIf True thenFunc elseFunc = thenFunc
myIf False thenFunc elseFunc = elseFunc

main =
  let x = 5
  in print $ myIf (x == 5) "is five" "is not five"
```

> `myIf` has three arguments; the first one is a boolean type argument;
> the `"is five"` in the main function is the `thenFunc` param;
> the `"is not five"` in the main function is the `elseFunc` param.
>
> Therefore, the `myIf` has two possible cases using "past passing".

### Strong and Static Type System

- Catch errors at compile time
- Generate the most efficient code as possible
- _Types are deleted at compile time_
  - Haskell aims to reduce the number of program that despite not being well typed still works
  - Maximize expressivity
  - Minimize program a frustration

### Summary

- whitespace sensitive
- mostly no curly braces
- [NEED CLARIFICATIONS] ghc runtime as a garbage collector? due to the pure data structure and static typing
- [NEED CLARIFICATIONS] memory allocations, external/internal???
- [NEED CLARIFICATIONS] class, data type, instance, module
