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
