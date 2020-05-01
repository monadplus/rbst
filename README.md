## Randomized Binary Search Trees (RBSTs)

![RBST nodes](./images/rbst.png)
[![Build Status](https://travis-ci.org/monadplus/RBST.svg?branch=master)](https://travis-ci.org/monadplus/RBST)

Efficient implementation of the [Randomized Binary Search Trees][1] data structure.

### What does this package provide?

This package implements a self-balancing-tree-like data structure called _Randomized Binary Search Tree_.

This data structure behave exactly like a _Random Binary Search Tree_, irrespectively of the input data distribution, with fast (logarithmic time complexity) `insert`,`delete`,`lookup`,`merge` operations.

### When to use this package?

_Randomized Binary Search Trees_ are useful when you __cannot make assumptions on the input distribution__. It is guaranteed efficient self-balancing. Below you can find the table of time complexity for all operations (where `n` is the size of the tree):

| Operation | Time complexity |
|-----------|-----------------|
| `size`    | `O(1)`          |
| `insert`  | `O(log n)`      |
| `delete`  | `O(log n)`      |
| `lookup`  | `O(log n)`      |
| `merge`   | `O(log n)`      |

### Technical background

All self-balancing binary search trees such as _AVLs_, _Red-black trees_, _BB[α]-tree_, _height-ratio-balanced tree_, etc. may suffer from the following:

- `UPDATE`/`DELETE` algorithms are rather complex (requires complex rotations).
- `UPDATE/DELETE` are _O(log n + c)_, where the constant factor can become large with ease.
- Tree nodes are required to store balance information needed only to preserve constraints.

A _Randomized BST_ is a _Random BST_ (as proved in the paper), irrespectively of the order of `INSERTION`/`DELETE`. Consequently, the performance is always, with a _constant_ factor, __logarithmic__. For example, an attacker could not force it to be imbalanced.

One of the keys of _RBST_ is that it __always__ generates a _Random Binary Search Tree_, so you can take advantage of all the properties of the _Random Binary Search Tree_.

#### RBSTs vs Randomized Treaps

- RBST generates random integers in the range of [0, _n_], where _n_ is the current size of the RBSTs, while randomized treaps produce unbounded integers in the unit interval (random priorities).

- SEARCH/DELETE are done by structural information (rank of subtree) in RBST, while on Randomized Treaps are done using nonstructural information, _random priority [0,1]_.

### Acknowledgement

To [C.Martinez](https://www.cs.upc.edu/~conrado/) and [S.Roura](https://www.cs.upc.edu/~roura/) for their work.

To [Dmitrii Kovanikov](https://github.com/chshersh) for his project [implicit treap][2], which has been a reference for me.

[1]: http://akira.ruc.dk/~keld/teaching/algoritmedesign_f08/Artikler/03/Martinez97.pdf
[2]: https://github.com/chshersh/treap
