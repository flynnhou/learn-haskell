# 14. Algebraic Data Types

## Table of Contents

- What is an algebraic data types
- Sum types
- Product types
- Record syntax
- Predefined algebraic data types

## What is an algebraic data types

- Do not confuse them with _abstract data types_ (ABT)
- Composite data types
- Sum types
- Product types

### Compare with C++ Enum

```cpp
enum class Color {
    Red,
    Green,
    Blue
};
```

```hs
module ColorSimpleSumType (Color) where

data Color = Red | Green | Blue deriving Show
```

```bash
ghci> :load ColorSimpleSumType.hs 
[1 of 1] Compiling ColorSimpleSumType ( ColorSimpleSumType.hs, interpreted )
Ok, one module loaded.
```

```bash
ghci> :info Color
type Color :: *
data Color = Red | Green | Blue
        -- Defined at ColorSimpleSumType.hs:3:1
instance [safe] Show Color -- Defined at ColorSimpleSumType.hs:3:42
```

```bash
data Color = Red | ...
        -- Defined at ColorSimpleSumType.hs:3:14
ghci> :info Green
type Color :: *
data Color = ... | Green | ...
        -- Defined at ColorSimpleSumType.hs:3:20
ghci> :info Blue
type Color :: *
data Color = ... | Blue
        -- Defined at ColorSimpleSumType.hs:3:28
```

```bash
ghci> x = Red
ghci> :type x
x :: Color
ghci> x
Red
```

##### Data Type - Bool

```bash
ghci> :info Bool
type Bool :: *
data Bool = False | True
        -- Defined in 'GHC.Types'
instance Eq Bool -- Defined in 'GHC.Classes'
instance Ord Bool -- Defined in 'GHC.Classes'
instance Enum Bool -- Defined in 'GHC.Enum'
instance Show Bool -- Defined in 'GHC.Show'
instance Read Bool -- Defined in 'GHC.Read'
instance Bounded Bool -- Defined in 'GHC.Enum'
```

### Compare with C++ Struct

```cpp
struct Color {
    int red;
    int green;
    int blue;
};
```

```hs
module ColorRGB (Color) where

data Color = RGB Int Int int deriving Show
```

```bash
ghci> :load ColorRGB.hs 
[1 of 1] Compiling ColorRGB         ( ColorRGB.hs, interpreted )
Ok, one module loaded.
```

```bash
ghci> :info Color
type Color :: *
data Color = RGB Int Int Int
        -- Defined at ColorRGB.hs:3:1
instance [safe] Show Color -- Defined at ColorRGB.hs:3:39
ghci> :info RGB
type Color :: *
data Color = RGB Int Int Int
        -- Defined at ColorRGB.hs:3:14
ghci> x = RGB 10 20 30
ghci> x
RGB 10 20 30
ghci> :type x
x :: Color
```

```hs
module ColorRGBRecord (Color) where

data Color = RGB
    { red :: Int
    , green :: Int
    , blue :: Int
    } deriving Show
```

```bash
ghci> :load ColorRGBRecord.hs 
[1 of 1] Compiling ColorRGBRecord   ( ColorRGBRecord.hs, interpreted )
Ok, one module loaded.
```

```bash
ghci> :info Color
type Color :: *
data Color = RGB {red :: Int, green :: Int, blue :: Int}
        -- Defined at ColorRGBRecord.hs:3:1
instance [safe] Show Color -- Defined at ColorRGBRecord.hs:7:16
ghci> :info RGB
type Color :: *
data Color = RGB {...}
        -- Defined at ColorRGBRecord.hs:3:14
ghci> :type red
red :: Color -> Int
ghci> :type green
green :: Color -> Int
ghci> :type blue
blue :: Color -> Int
ghci> x = RGB 10 20 30
ghci> x
RGB {red = 10, green = 20, blue = 30}
ghci> red x
10
ghci> green x
20
ghci> blue x
30
```

##### Record Update

```bash
ghci> y = x { green = 40 }
ghci> x
RGB {red = 10, green = 20, blue = 30}
ghci> y
RGB {red = 10, green = 40, blue = 30}
```

### Compare C++ Data Type Overload

```cpp
#includ <casset>
using namespace std;

struct RGB { int red; int green; int blue; }
struct CMYK { float cyan; float magenta; float yellow; float key; };

class Color {
public:
    Color(int r, int g, int b) : type(Type::RGB) { value.rgb = RGB{ r, g, b }; }
    Color(float c, float m, float y, float k) : type(Type::CMYK) { value.cmyk = CMYK{ c, m, y, k }; }
    
    enum class Type { RGB, CMYK } type;
    const RGB& rgb() const { asset(type == Type::RGB); return value.rgb; }
    const CMYK& cmyk() const { asset(type == Type::CMYK); return value.cmyk; }
private:
    union { RGB rgb; CMYK cmyk; } value;
};
```

```bash
ghci> :load ColorRGBCMYK.hs 
[1 of 1] Compiling ColorRGBCMYK     ( ColorRGBCMYK.hs, interpreted )
Ok, one module loaded.
```

```bash
ghci> :info Color
type Color :: *
data Color = RGB Int Int Int | CMYK Float Float Float Float
        -- Defined at ColorRGBCMYK.hs:3:1
instance [safe] Show Color -- Defined at ColorRGBCMYK.hs:3:70
ghci> :info RGB
type Color :: *
data Color = RGB Int Int Int | ...
        -- Defined at ColorRGBCMYK.hs:3:14
ghci> :info CMYK
type Color :: *
data Color = ... | CMYK Float Float Float Float
        -- Defined at ColorRGBCMYK.hs:3:32
ghci> x = RGB 11 22 33
ghci> :type x
x :: Color
ghci> x
RGB 11 22 33
ghci> y = CMYK 1.0 2.0 3.0 4.0
ghci> :type y
y :: Color
ghci> y
CMYK 1.0 2.0 3.0 4.0
ghci>
```
