vector_mul [] [] acc = acc
vector_mul v_first v_second acc = vector_mul tail_v_first tail_v_second new_acc
    where new_acc                       = head_v_first * head_v_second + acc
          (head_v_first:tail_v_first)   = v_first
          (head_v_second:tail_v_second) = v_second

matrix_mul_by_col :: Num a => [[a]] -> [a] -> [a]
matrix_mul_by_col matrix col = [ vector_mul row col 0 | row <- matrix ]

matrix_push_last_col matrix col = [ current_row ++ [col_elem] | (current_row, col_elem) <- zip matrix col ]

matrix_pop_first_col :: Num a => [[a]] -> ([a], [[a]])
matrix_pop_first_col matrix
    | all null matrix   = ([], matrix)
    | otherwise         = unzip [ (elem, row) | (elem : row) <- matrix ]

matrix_mul m_first m_second = matrix_mul_helper m_first m_second (take (length m_first) $ repeat [])

matrix_mul_helper m_first m_second m_acc
    | all null m_second     = m_acc
    | otherwise             = matrix_mul_helper m_first cropped m_new_acc
        where m_new_acc          = matrix_push_last_col m_acc res_col
              res_col            = matrix_mul_by_col m_first cur_col
              (cur_col, cropped) = matrix_pop_first_col m_second

matrix_identity :: Num a => Int -> [[a]]
matrix_identity n = [ (take prep_count $ repeat 0) ++ [1] ++ (take app_count $ repeat 0) |
    (prep_count, app_count) <- zip [0 .. n - 1] [n - 1, n - 2 .. 0] ]

matrix_log_pow :: (Num a, Integral b) => b -> [[a]] -> [[a]]
matrix_log_pow 0 matrix = matrix_identity $ length matrix
matrix_log_pow power matrix = case divMod power 2 of
    (_, 1)  -> matrix_mul matrix (matrix_log_pow (power - 1) matrix)
    (p, _)  -> matrix_log_pow p $ matrix_mul matrix matrix

log_fib 0 = 0
log_fib 1 = 1
log_fib n = last $ last $ matrix_log_pow (n - 1) [ [0, 1], [1, 1] ]
