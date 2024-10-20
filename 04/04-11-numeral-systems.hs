fromDigits base digits = evalPolynomial_h digits base 0

evalPolynomial_h [] _ res = res
evalPolynomial_h (c:coeficients) value res = evalPolynomial_h coeficients value (res * value + c)


toDigits base value = _toDigits_h base value []

_toDigits_h _ 0 acc_list = acc_list
_toDigits_h base value acc_list = let (d, m) = divMod value base in
    _toDigits_h base d (m : acc_list)


addDigitwise base l1 l2 = _addDigitwise_h base (reverse l1) (reverse l2) [] False

_addDigitwise_h _    [] [] acc carry = if carry then (1 : acc) else acc
_addDigitwise_h base [] l2 acc carry = _addDigitwise_h base [0] l2 acc carry
_addDigitwise_h base l1 [] acc carry = _addDigitwise_h base l1 [0] acc carry
_addDigitwise_h base l1 l2 acc carry =
    let (h1 : t1) = l1
        (h2 : t2) = l2
        sum = h1 + h2 + if carry then 1 else 0
    in if sum >= base
        then _addDigitwise_h base t1 t2 (sum - base : acc) True
        else _addDigitwise_h base t1 t2 (sum : acc) False

