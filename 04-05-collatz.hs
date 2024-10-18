collatz :: Integer -> Integer
collatz n = collatz_h n 1

collatz_h 1 acc = acc
collatz_h n acc = collatz_h (let (d, m) = divMod n 2 in if m == 0 then d else 3 * n + 1) (acc + 1)

