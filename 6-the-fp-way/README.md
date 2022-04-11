# 6. The FP Way

### Compare C++ programs with haskell fp

###### traversal

```cpp
#include <iostream>
#include <vector>
using namespace std;

int main() {
  vector<int> xs{1, 2, 3};
  vector<int> ys;
  for (auto x : xs) {
    ys.emplace_back(x + 10);
  }

  for (auto y: ys) {
    cout << y << endl;
  }
}
```

```haskell
# Traversal.hs
main = print $ map (+ 10) [1, 2, 3]
```

```bash
stack ghc -- -o TraversalHS Traversal.hs
```

###### reduce

```cpp
#include <iostream>
#include <vector>
using namspace std;

int main() {
  vector<int> xs{1, 2, 3};
  int total = 0;
  for (auto x : xs) {
    total += x;
  }
  cout << total << endl;
}
```

```hs
Reduction.hs
main = print $ foldr (+) 0 [1, 2, 3]
```

> `(+)` the operand; `0` the starting value; `[1, 2, 3]` the value to foldr against

```bash
stack ghc -- -o ReductionHS Reduction.hs
```

###### filter

```cpp
#include <iostream>
#include <vector>
using namspace std;

int main() {
  vector<int> xs{1, 2, 3, 4, 5, 6};
  vector<int> ys;
  for (auto x : xs) {
    if (x > 3) {
      ys.emplace_back(x);
    }
  }
  
  for (auto y : ys) {
    cout << y << endl;
  }
}
```

```hs
main = print $ filter (>3) [1, 2, 3, 4, 5, 6]
```

> `(>3)` predicate; `[1, 2, 3, 4, 5, 6]` the value to filter against


###### composition

```cpp
#include <iostream>
#include <vector>
using namspace std;

int f(int x) { return x + 10; }

int g(int x) { return x * x; }

int main() {
  vector<int> xs{1, 2, 3};
  vector<int> ys;
  for (auto x : xs) {
    ys.emplace_back(f(g(x)));
  }
  for (auto y : ys) {
    cout << y << endl;
  }
}
```

```hs
f x = x + 10
g x = x * x
main = print $ map (g . f) [1, 2, 3]
```
