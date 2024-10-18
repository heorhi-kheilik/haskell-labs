xZipWith func l1 l2 = _xZipWith_h func l1 l2 []

_xZipWith_h func l1 l2 l_res
    | null l1   = l_res
    | null l2   = l_res
    | otherwise = _xZipWith_h func t_l1 t_l2 (l_res ++ [func h_l1 h_l2])
        where (h_l1 : t_l1) = l1
              (h_l2 : t_l2) = l2
