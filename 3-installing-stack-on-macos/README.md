# 3. Installing Stack on Mac OS

### Define stack resolver globally

> This step is different from the lecturer's way of specifying resolver with the `new` command.

```bash
stack config set resolver lts
```

### Write a hello-world program

```bash
stack new hello-world simple
```

> C.f., from the course, `stack new hello-world simple --resolver=lts-7.8`

```
Downloading template "simple" to create project "hello-world" in hello-world/ ...

The following parameters were needed by the template but not provided: author-name
You can provide them in /Users/flynnhou/.stack/config.yaml, like this:
templates:
  params:
    author-name: value
Or you can pass each one as parameters like this:
stack new hello-world simple -p "author-name:value"


The following parameters were needed by the template but not provided: author-email, author-name, category, copyright, github-username
You can provide them in /Users/flynnhou/.stack/config.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
    category: value
    copyright: value
    github-username: value
Or you can pass each one as parameters like this:
stack new hello-world simple -p "author-email:value" -p "author-name:value" -p "category:value" -p "copyright:value" -p "github-username:value"

Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- hello-world/

Selecting the best among 21 snapshots...

* Matches https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/19/2.yaml

Selected resolver: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/19/2.yaml
Initialising configuration using resolver: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/19/2.yaml
Total number of user packages considered: 1
Writing configuration to file: hello-world/stack.yaml
All done.
/Users/flynnhou/.stack/templates/simple.hsfiles:    4.71 KiB downloaded...
```

### Build the hello-world from src

```bash
cd hello-world
stack build
```

```
[1 of 2] Compiling Main             ( /Users/flynnhou/.stack/setup-exe-src/setup-mPHDZzAJ.hs, /Users/flynnhou/.stack/setup-exe-src/setup-mPHDZzAJ.o )
[2 of 2] Compiling StackSetupShim   ( /Users/flynnhou/.stack/setup-exe-src/setup-shim-mPHDZzAJ.hs, /Users/flynnhou/.stack/setup-exe-src/setup-shim-mPHDZzAJ.o )
Linking /Users/flynnhou/.stack/setup-exe-cache/x86_64-osx/tmp-Cabal-simple_mPHDZzAJ_3.4.1.0_ghc-9.0.2 ...
Building all executables for `hello-world' once. After a successful build of all of them, only specified executables will be rebuilt.
hello-world> configure (exe)
Configuring hello-world-0.1.0.0...
hello-world> build (exe)
Preprocessing executable 'hello-world' for hello-world-0.1.0.0..
Building executable 'hello-world' for hello-world-0.1.0.0..
[1 of 1] Compiling Main
Linking .stack-work/dist/x86_64-osx/Cabal-3.4.1.0/build/hello-world/hello-world ...
hello-world> copy/register
Installing executable hello-world in /Users/flynnhou/repos/personal/learn-haskell/udemy/3-installing-stack-on-macos/hello-world/.stack-work/install/x86_64-osx/fb62044d263f47265bdd8dd1276be62bc9d964c34a8ee6ec44f41e2ae43c7a25/9.0.2/bin
```

### Setup GHC

> I skipped this step because I installed stack from ghc a while ago.

```bash
stack setup
```

### Run hello-world

- Way 1 - run from the absolute path of the binary
  ```bash
  /Users/flynnhou/repos/personal/learn-haskell/udemy/3-installing-stack-on-macos/hello-world/.stack-work/install/x86_64-osx/fb62044d263f47265bdd8dd1276be62bc9d964c34a8ee6ec44f41e2ae43c7a25/9.0.2/bin/hello-world
  ```
- Way 2 - run `exec` under the src folder
  ```bash
  stack exec hello-world
  ```

