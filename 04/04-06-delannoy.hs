delannoy 0 k = k
delannoy k 0 = k
delannoy m n = sum [delannoy (m - 1) n, delannoy m (n - 1), delannoy (m - 1) (n - 1)]
