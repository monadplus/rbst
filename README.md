## rbst

![RBST nodes](./images/rbst.png)
[![Build Status](https://travis-ci.org/monadplus/RBST.svg?branch=master)](https://travis-ci.org/monadplus/RBST)

Efficient implementation of the [Randomized Binary Search Trees][1] data structure.

### What does this package provide?

This package implements a self-balancing-tree-like data structure called _Randomized Binary Search Tree_.

This data structure behave exactly like a _Random Binary Search Tree_, irrespectively of the input data distribution, with fast (logarithmic time complexity) `insert`,`delete`,`lookup`,`union` operations.

### When to use this package?

_Randomized Binary Search Trees_ are useful when you __cannot make assumptions on the input distribution__. It is guaranteed efficient self-balancing. Below you can find the table of time complexity for all operations (where `n` is the size of the tree):

| Operation | Time complexity |
|-----------|-----------------|
| `size`    | `O(1)`          |
| `lookup`  | `O(log n)`      |
| `insert`  | `O(log n)`      |
| `delete`  | `O(log n)`      |
| `union`   | `O(m + n)`      |

### Acknowledgement

To [C.Martinez](https://www.cs.upc.edu/~conrado/) and [S.Roura](https://www.cs.upc.edu/~roura/) for their work.

To [Dmitrii Kovanikov](https://github.com/chshersh) for his project [implicit treap][2], which has been a reference for me.

[1]: http://akira.ruc.dk/~keld/teaching/algoritmedesign_f08/Artikler/03/Martinez97.pdf
[2]: https://github.com/chshersh/treap
