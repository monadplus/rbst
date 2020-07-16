bst join(bst L, bst R) {
  int m, n, r, total;

  m = L->size; n = R->suze; total = m + n;
  if (total == 0) return EMPTY;
  r = random(0, total - 1);
  if (r < m) { /* with probability m / (m+n) */
    L->right = join(L->right, R);
    return L;
  }
  else {  /* with probability n / (m+n) */
    R->left = join(L, R->left);
    return R;
  }
}
