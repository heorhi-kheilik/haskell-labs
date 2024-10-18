fib_by_count 0 = []
fib_by_count 1 = [0]
fib_by_count count = reverse $ _fib_by_count_helper [1,0] (count - 2)

_fib_by_count_helper list 0 = list
_fib_by_count_helper list@(second:first:_) count = _fib_by_count_helper (new:list) (count - 1)
    where new = first + second

fib_list = [0, 1] ++ generate 0 1
    where generate first second = let new_n = first + second in [new_n] ++ generate second new_n

generalized_fibonacci list = list ++ (generate list)
    where generate list = let new_n = sum list in [new_n] ++ generate (tail list ++ [new_n])
