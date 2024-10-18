-- [
--     [1, 2, 3],
--     [4, 5, 6]
-- ]

-- [
--     [1, 2],
--     [3, 4],
--     [5, 6]
-- ]

-- matrix_mul :: Num a => [[a]] -> [[a]] -> [[a]]
-- matrix_mul m_first m_second = let i = [0..length matrix]
--                                   j = [0..length matrix !! 0] in

-- matrix_calc_mul_upper_left matrix = 

vector_mul [] [] acc = acc
vector_mul v_first v_second acc = vector_mul tail_v_first tail_v_second new_acc
    where new_acc                       = head_v_first * head_v_second + acc
          (head_v_first:tail_v_first)   = v_first
          (head_v_second:tail_v_second) = v_second

matrix_mul_by_col :: Num a => [[a]] -> [a] -> [a]
matrix_mul_by_col matrix col = [ vector_mul row col 0 | row <- matrix ]

matrix_add_col matrix col = [ current_row ++ [col_elem] | (current_row, col_elem) <- zip matrix col ]




-- matrix_drop_row :: Num a => [[a]] -> [[a]]
-- matrix_drop_row (row:other) = other

-- matrix_drop_col :: Num a => [[a]] -> [[a]]
-- matrix_drop_col matrix = [ tail row | row <- matrix ]




-- matrix_calc_mul_element :: Num a, Integral b => [a] -> b -> b -> a
-- matrix_calc_mul_element matrix i j = sum [ x * y | (x, y) <- zip row col ]
--     where row = matrix !! i
--           col = get_col matrix j
--           get_col matrix index = [ row !! index | row <- matrix ]

-- matrix_get_row matrix index = matrix !! index

-- matrix_get_col matrix index = [ row !! index | row <- matrix ]
