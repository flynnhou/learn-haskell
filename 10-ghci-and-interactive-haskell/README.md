# 10. GHCi and Interactive Haskell

### Names

1. **GHC**: Glasgow Haskell Compiler aka _The Glorious Glasgow Haskell Compilation System_
2. **GHCi**: Glasgow Haskell Compiler Interactive

### Prompt

```bash
ghci> :?
 Commands available from the prompt:

   <statement>                 evaluate/run <statement>
   :                           repeat last command
   :{\n ..lines.. \n:}\n       multiline command
   :add [*]<module> ...        add module(s) to the current target set
   :browse[!] [[*]<mod>]       display the names defined by module <mod>
                               (!: more details; *: all top-level names)
   :cd <dir>                   change directory to <dir>
   :cmd <expr>                 run the commands returned by <expr>::IO String
   :complete <dom> [<rng>] <s> list completions for partial input string
   :ctags[!] [<file>]          create tags file <file> for Vi (default: "tags")
                               (!: use regex instead of line number)
   :def[!] <cmd> <expr>        define command :<cmd> (later defined command has
                               precedence, ::<cmd> is always a builtin command)
                               (!: redefine an existing command name)
   :doc <name>                 display docs for the given name (experimental)
   :edit <file>                edit file
   :edit                       edit last module
   :etags [<file>]             create tags file <file> for Emacs (default: "TAGS")
   :help, :?                   display this list of commands
   :info[!] [<name> ...]       display information about the given names
                               (!: do not filter instances)
   :instances <type>           display the class instances available for <type>
   :issafe [<mod>]             display safe haskell information of module <mod>
   :kind[!] <type>             show the kind of <type>
                               (!: also print the normalised type)
   :load[!] [*]<module> ...    load module(s) and their dependents
                               (!: defer type errors)
   :main [<arguments> ...]     run the main function with the given arguments
   :module [+/-] [*]<mod> ...  set the context for expression evaluation
   :quit                       exit GHCi
   :reload[!]                  reload the current module set
                               (!: defer type errors)
   :run function [<arguments> ...] run the function with the given arguments
   :script <file>              run the script <file>
   :type <expr>                show the type of <expr>
   :type +d <expr>             show the type of <expr>, defaulting type variables
   :type +v <expr>             show the type of <expr>, with its specified tyvars
   :unadd <module> ...         remove module(s) from the current target set
   :undef <cmd>                undefine user-defined command :<cmd>
   ::<cmd>                     run the builtin command
   :!<command>                 run the shell command <command>

 -- Commands for debugging:

   :abandon                    at a breakpoint, abandon current computation
   :back [<n>]                 go back in the history N steps (after :trace)
   :break [<mod>] <l> [<col>]  set a breakpoint at the specified location
   :break <name>               set a breakpoint on the specified function
   :continue                   resume after a breakpoint
   :delete <number> ...        delete the specified breakpoints
   :delete *                   delete all breakpoints
   :disable <number> ...       disable the specified breakpoints
   :disable *                  disable all breakpoints
   :enable <number> ...        enable the specified breakpoints
   :enable *                   enable all breakpoints
   :force <expr>               print <expr>, forcing unevaluated parts
   :forward [<n>]              go forward in the history N step s(after :back)
   :history [<n>]              after :trace, show the execution history
   :list                       show the source code around current breakpoint
   :list <identifier>          show the source code for <identifier>
   :list [<module>] <line>     show the source code around line number <line>
   :print [<name> ...]         show a value without forcing its computation
   :sprint [<name> ...]        simplified version of :print
   :step                       single-step after stopping at a breakpoint
   :step <expr>                single-step into <expr>
   :steplocal                  single-step within the current top-level binding
   :stepmodule                 single-step restricted to the current module
   :trace                      trace after stopping at a breakpoint
   :trace <expr>               evaluate <expr> with tracing on (see :history)

 -- Commands for changing settings:

   :set <option> ...           set options
   :seti <option> ...          set options for interactive evaluation only
   :set local-config { source | ignore }
                               set whether to source .ghci in current dir
                               (loading untrusted config is a security issue)
   :set args <arg> ...         set the arguments returned by System.getArgs
   :set prog <progname>        set the value returned by System.getProgName
   :set prompt <prompt>        set the prompt used in GHCi
   :set prompt-cont <prompt>   set the continuation prompt used in GHCi
   :set prompt-function <expr> set the function to handle the prompt
   :set prompt-cont-function <expr>
                               set the function to handle the continuation prompt
   :set editor <cmd>           set the command used for :edit
   :set stop [<n>] <cmd>       set the command to run when a breakpoint is hit
   :unset <option> ...         unset options

  Options for ':set' and ':unset':

    +m            allow multiline commands
    +r            revert top-level expressions after each evaluation
    +s            print timing/memory stats after each evaluation
    +t            print type after evaluation
    +c            collect type/location info after loading modules
    -<flags>      most GHC command line flags can also be set here
                         (eg. -v2, -XFlexibleInstances, etc.)
                    for GHCi-specific flags, see User's Guide,
                    Flag reference, Interactive-mode options

 -- Commands for displaying information:

   :show bindings              show the current bindings made at the prompt
   :show breaks                show the active breakpoints
   :show context               show the breakpoint context
   :show imports               show the current imports
   :show linker                show current linker state
   :show modules               show the currently loaded modules
   :show packages              show the currently active package flags
   :show paths                 show the currently active search paths
   :show language              show the currently active language flags
   :show targets               show the current set of targets
   :show <setting>             show value of <setting>, which is one of
                                  [args, prog, editor, stop]
   :showi language             show language flags for interactive evaluation

 The User's Guide has more information. An online copy can be found here:

   https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html

ghci> 
```

### `:!<command>` Run the Shell Command <command>

> `:!` read as "colon bang"

```bash
ghci> :!ls
README.md
ghci> :!mkdir TEMP
ghci> :!ls
README.md TEMP
ghci> :!rm -rf TEMP/
ghci> :!ls
README.md
ghci>
```

### `:type <expr>` Show the Type of <expr>

```bash
ghci> x = 5
ghci> y = 6
ghci> :type x
x :: Num p => p
ghci> :type y
y :: Num p => p
ghci> x + y
11
ghci> it
11
ghci> :type it
it :: Num a => a
ghci>
ghci> :type Num

<interactive>:1:1: error:
    * Data constructor not in scope: Num
    * Perhaps you meant variable 'sum' (imported from Prelude)
ghci> :type sum
sum :: (Foldable t, Num a) => t a -> a
```

- `ghci` binds the name `it` to the most recent evaluated result
- `num` is not a value but a type. Types have kinds.

### `:kind <type>` Show the Kind of <type>

```bash
ghci> :kind Num
Num :: * -> Constraint
```

### `:info[!] [<name> ...]` Display Information of the Given Names

```bash
ghci> :info it
it :: Num a => a        -- Defined at <interactive>:33:1
ghci> :info Num
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
        -- Defined in 'GHC.Num'
instance Num Word -- Defined in 'GHC.Num'
instance Num Integer -- Defined in 'GHC.Num'
instance Num Int -- Defined in 'GHC.Num'
instance Num Float -- Defined in 'GHC.Float'
instance Num Double -- Defined in 'GHC.Float'
```

### Inspect and Run a File

##### Catalogue the File

```bash
ghci> :!cat Ipv4.hs
module Main (main, prettyPrint) where

type Port = Int

data Address = Address Int Int Int Int Port

prettyPrint :: Address -> IO ()
prettyPrint (Address ip0 ip1 ip2 ip3 port)
    = putStrLn $
        show ip0 ++ "." ++
        show ip1 ++ "." ++
        show ip2 ++ "." ++
        show ip3 ++ ":" ++ show port

main :: IO ()
main = prettyPrint (Address 127 0 0 1 80)
```

##### Load the File into the Prompt

```bash
ghci> :load Program
[1 of 1] Compiling Main             ( Ipv4.hs, interpreted )
Ok, one module loaded.
```

##### Use `:kind`, `:type`, `:info`

```bash
ghci> :kind Port
Port :: *
ghci>
ghci> :info Port
type Port :: *
type Port = Int
        -- Defined at Ipv4.hs:14:1
ghci>
ghci> :kind Address
Address :: *
ghci>
ghci> :type prettyPrint
prettyPrint :: Address -> IO ()
ghci>
ghci> :info prettyPrint
prettyPrint :: Address -> IO ()         -- Defined at Ipv4.hs:19:1
ghci>
ghci> :type main
main :: IO ()
ghci>
ghci> :info main
main :: IO ()   -- Defined at Ipv4.hs:27:1
```

##### Run the `main`

The following command has the same effect.

```bash
ghci> :main
127.0.0.1:80
ghci> :run main
127.0.0.1:80
ghci> main
127.0.0.1:80
```

##### Edit the Function of the File

```bash
ghci> prettyPrint (Address 1 2 3 4 5)
1.2.3.4:5
ghci> :edit  # open the default text editor (in my case, vim)
[1 of 1] Compiling Main             ( Ipv4.hs, interpreted )
Ok, one module loaded.
```

```bash
ghci> prettyPrint (Address 1 2 3 4 5)
(IPv4) 1.2.3.4:5
```

##### Reload and Browse Module

```bash
ghci> :reload
Ok, one module loaded.
ghci> :r
Ok, one module loaded.
ghci> 
ghci> :browse Main
prettyPrint :: Address -> IO ()
main :: IO ()
ghci> :bro Main
prettyPrint :: Address -> IO ()
main :: IO ()
ghci> 
```

## Resources

- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html
