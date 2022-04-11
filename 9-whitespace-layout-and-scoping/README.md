# 9. Whitespace, Layout, and Spacing

### Program.hs

- Compile `Program.hs` without generating an output file
  ```bash
  stack ghc -- -fno-code Ipv4.hs
  ```
  
  > `stack ghc`: Utilize `stack` to invoke `ghc`
  > 
  > `--`: Pass the rest flags to `ghc`
  > 
  > `-fno-code`: Omit code generation (according to `man ghc`)
