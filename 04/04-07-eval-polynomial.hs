evalPolynomial coeficients value = evalPolynomial_h coeficients value 0

evalPolynomial_h [] _ res = res
evalPolynomial_h (c:coeficients) value res = evalPolynomial_h coeficients value (res * value + c)

