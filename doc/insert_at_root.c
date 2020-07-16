bst insert_at_root(int x, bst T) {
  bst S, G;

  split(x, T, &S, &G);
  T = new_node();
  T->key = x; T->left = S; T->right = G;
  return T;
}

void split (int x, bst T, bst *S, bst *G){
  if (T == EMPTY) {
    *S = *G = EMPTY;
    return;
  }
  if (x < T->key) {
    *G = T;
    split(x, T->left, S, &(*G->left));
  }
  else { /* x > T->key*/
    *S = T;
    split(x, T->right, &(*S->right), G);
  }
  return;
}
