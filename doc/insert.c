bst insert(int x, bst T) {
  int n, r;

  n = T->size;
  r = random(0,n);
  if (r == n)
    return insert_at_root(x,T);
  if (x < T->key)
    T->left = insert(x, T->left);
  else
    T->right = insert(x, T->right);
  return T;
}
