## rbst: efficient randomized binary search trees

![RBST nodes](./images/rbst.png)
[![Hackage](https://img.shields.io/hackage/v/rbst.svg)](https://hackage.haskell.org/package/rbst)
[![GitHub CI](https://github.com/monadplus/rbst/workflows/CI/badge.svg)](https://github.com/monadplus/rbst/actions)
[![Build Status](https://travis-ci.org/monadplus/RBST.svg?branch=master)](https://travis-ci.org/monadplus/RBST)
[![MIT license](https://img.shields.io/github/license/monadplus/rbst)](LICENSE)

Efficient implementation of the [Randomized Binary Search Trees][1] data structure.

### When to use this package?

_Randomized Binary Search Trees_ are useful when you __cannot make assumptions on the input distribution__ but you still need fast (logarithmic time complexity) `insert`/`lookup`/`delete`/`at`/`union`/etc. operations. It is guaranteed efficient self-balancing. Below you can find the table of time complexity for all operations (where `n` is the size of the tree):

| Operation      | Time complexity  | Description                                    |
|----------------|------------------|------------------------------------------------|
| `size`         | `O(1)`           | Count elements in the tree                     |
| `height`       | `O(log n)` w.h.p | Height of the tree                             |
| `lookup`       | `O(log n)`       | Access by key                                  |
| `insert`       | `O(log n)`       | Insert an element with the given key           |
| `delete`       | `O(log n)`       | Delete the element associated to the given key |
| `take`         | `O(log n)`       | Take first i-th elements                       |
| `drop`         | `O(log n)`       | Drop first i-th elements                       |
| `at`           | `O(log n)`       | Access by index                                |
| `remove`       | `O(log n)`       | Remove the i-th element                        |
| `union`        | `O(m + n)`       | Union of two trees                             |
| `intersection` | `O(m + n)`       | Intersection of two trees                      |
| `subtraction`  | `O(m + n)`       | Subtraction of two trees                       |
| `difference`   | `O(m + n)`       | Symmetric difference of two trees              |

### Usage example

```haskell
>>> :set -XOverlodadeLists
>>> import GHC.Exts
>>> import RBST

-- Manually created
>>> let tree =  insert 'a' 1
              $ insert 'b' 2
              $ insert 'c' 3
              $ insert 'd' 4
              $ insert 'e' 5
              $ empty

-- Using 'OverloadedLists'
>>> let tree = (fromList $ zip ['a'..'e'] [1..5]) :: RBST Char Int
>>> RBST.prettyPrint tree
         ('b',2) [5]
                 ╱╲
                ╱  ╲
               ╱    ╲
              ╱      ╲
             ╱        ╲
            ╱          ╲
           ╱            ╲
          ╱              ╲
         ╱                ╲
('a',1) [1]       ('d',4) [3]
                       ╱╲
                      ╱  ╲
                     ╱    ╲
                    ╱      ╲
                   ╱        ╲
                  ╱          ╲
            ('c',3) [1] ('e',5) [1]

-- Queries
>>> size tree
5
>>> lookup 'd'
Just 4
>>> lookup 'a' $ insert 'a' 7 tree
Just 7
>>> lookup 'd' (delete 'd' tree)
Nothing
```

### How to use it

In order to start using `rbst` in your project, you will need to set it up with the two easy steps:

1. Add the dependency in your project's `.cabal` file:

   ```haskell
    build-depends: base ^>= 4.14
                 , rbst ^>= 0.0.0.0
   ```

2. In the module where you wish to use `rbst`,  add the (qualified) import:

   ```haskell
   import qualified RBST
   ```

#### Stack

1. If `rbst` is not available on your current [Stackage][3] resolver yet, you can still use it by adding the following in the `extra-deps` section of your `stack.yaml` file:

    ```haskell
    extra-deps:
      - rbst-0.0.0.0
    ```

2. Then you can add it as a dependency in your `package.yaml` file as usual:

    ```
    library:
      dependencies:
        - rbst
    ```

### Acknowledgement

To the authors [C. Martinez](https://www.cs.upc.edu/~conrado/) and [S. Roura](https://www.cs.upc.edu/~roura/).

To [D. Kovanikov](https://github.com/chshersh) and his project [implicit treap][2].

Icons designed by [Freepik](http://www.freepik.com) from [www.flaticon.com](https://www.flaticon.com/).

[1]: https://www.cs.upc.edu/~conrado/research/papers/jacm-mr98.pdf
[2]: https://github.com/chshersh/treap
[3]: https://www.stackage.org/
