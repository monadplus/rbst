## Randomized Binary Search Trees (RBSTs)

### TODO

- [] Generalize keys (instead of Int).
- [] Handle insert duplicate items.

### Introduction

Original work: <http://akira.ruc.dk/~keld/teaching/algoritmedesign_f08/Artikler/03/Martinez97.pdf>

The original work has the followin sections:

- 2,3: insertion/delete operations
- 4: performance analysis
- 5: insertion of repeated keys, set operations over RBST, self-adjunt strategies.
- 6: implementation-related issues (optimization)
- 7: formal description of randomized algorithms
- 8: some remaks + future research

All self-balancing BST such as AVL, Red-black tree, BB[α] tree, height-ratio-balanced tree, etc. suffer from the following:

- UPDATE/DELETE algorithms are rather complex (requires complex rotations)>
- UPDATE/DELETE are O(log n + C), the constant factor can become large.
- Nodes require to store balance information needed only to preserve invariants/constraints.

A Randomized BST is a Random BST (as proved in the paper), irrespectively of the order of INSERTION/DELETE. Consequently, the performance is always (with high probability) logarithmic. For example, an attacker could not force it to be imbalanced.

One of the keys of RBST is that it always generates a Random Binary Search Tree, so you can take advantage of all the properties of the Random Binary Search Tree.

### RBSTs vs Randomized Treaps

- RBST generates random integers in the range of [0, _n_], where _n_ is the current size of the RBSTs, while randomized treaps produce unbounded integers in the unit interval (random priorities).
- SEARCH/DELETE are done by structural information (rank of subtree) in RBST, while on Randomized Treaps are done using nonstructural information, _random priority [0,1]_.
