-- 0 right
-- 1 up right
-- 2 up
--
-- n rows, m cols

delannoyPaths :: Int -> Int -> [[Int]]

delannoyPaths 0 0 = [[]]
delannoyPaths n 0 = [take n $ repeat 2]
delannoyPaths 0 m = [take m $ repeat 0]
delannoyPaths n m = concat [
        [ (2 : list) | list <- delannoyPaths (n - 1) m ],
        [ (0 : list) | list <- delannoyPaths n (m - 1) ],
        [ (1 : list) | list <- delannoyPaths (n - 1) (m - 1) ]
    ]
