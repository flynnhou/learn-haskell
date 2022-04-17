# 13. Types and Type Signatures

## Table of Contents

- Type Synonyms
- Type Signatures for Values
- Type Signatures for Functions
- Polymorphism
- Constraints

## Type Synonyms

> Also known as "alias"

```bash
ghci> :info Port
type Port :: *
type Port = Int
        -- Defined at <interactive>:46:1
```

```bash
ghci>
ghci> type HostInfo = (String, Int)
ghci> :info HostInfo
type HostInfo :: *
type HostInfo = (String, Int)
        -- Defined at <interactive>:51:1
```

```bash
ghci> type HostInfo = (String, Port)
ghci> :info HostInfo
type HostInfo :: *
type HostInfo = (String, Port)
        -- Defined at <interactive>:56:1
```

## Type Signatures for Values

```bash
stack runghc Nums.hs
5
5.0
2.0 :+ 3.0
2 % 3
```

```bash
stack runghc Chars.hs
X
X
X
X
```

```bash
stack runghc Strings.hs
abc
XXX
```

```bash
stack runghc Lists.hs
[1,2,3,4,5]
[1,2,3,4,5,6,7,8,9,10]
[1,3,5,7,9]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
["aaa","bbb","ccc","ddd"]
"abcd"
"abcd"
```

```bash
stack runghc Tuples.hs
(1234,5678)
("sometext",8,3.141)
([1,2,3,4],["aaa","bbb"],(1.0,'o'))
```

## Type Signatures for Functions

```bash
stack runghc Functions.hs
5
("hello",5)
[("Hello",5),("Goodbye",7)]
```

```bash
stack runghc FormatList.hs
<list>"first"|"second"|"third"|"fourth"</list>
```

## Polymorphism

```bash
stack runghc Polymorphism.hs 
["10","20","30"]
[10,20]
160
```

### Compare with C++

##### Inheritance

```cpp
class BaseClass {
public:
    virtual void method = 0;
};

class DerivedClass : public BaseClass {
public:
    void method() override { }
};

void f0(BaseClass& obj) {
    obj.method();
}

void f1() {
    DerivedClass obj;
    f0(obj);
}
```

##### Overloading

```cpp
#include <string>
using namespace std;

void func(int x) { }

void func(const string& x) { }

void func(double x) { }

void func(int x, int y, int z) { }

void example() {
    func(5);
    func("hello");
    func(5.0);
    func(5, 6, 7);
}
```

##### Templates

```cpp
#include <iostream>
using namespace std;

class C { };
bool operator>(const C& c0, const C& c1) { return false; }
ostream& operator<<(ostream& os, const C& c) { return os; }

template <typename T> T myMax(T a, T b) {
    return a > b ? a : b;
}

template <> const char* myMax(const char* a, const char* b) {
    return strcmp(a, b) > 0 ? a: b;
}

void f() {
    cout << myMax(9, 5) << endl;
    cout << myMax(9.0, 5.0) << endl;
    cout << myMax(C{ }, C{ }) << endl;
    cout << myMax("foo", "bar") << endl;
}
```

## Constraints

```bash
stack runghc Constraints.hs
60
```

Instead of

```hs
mySum :: Num a => [a] -> a
```

try **typed hole** (a hole in type signature)

```hs
mySum :: _
```

to get GHC monomorphic suggestion:

```bash
Constraints.hs:6:10: error:
    - Found type wildcard '_' standing for '[Integer] -> Integer'
      To use the inferred type, enable PartialTypeSignatures
    - In the type signature: mySum :: _
  |
6 | mySum :: _
  |          ^
```
