bst delete(int x, bst T){
  bst Aux;

  if (T == EMPTY)
    return EMPTY;
  if (x < T->key)
    T->left = delete(x, T->left);
  else if (x > T->key)
    T->right = delete(x, T->right);
  else { /* x == T->key */
    Aux = join(T->left, T->right);
    free_node(T);
    T = Aux;
  }
  return T;
}
