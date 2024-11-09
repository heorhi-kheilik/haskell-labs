foldToDecimal digits base = res where (res, _) = _foldToDecimalH (reverse $ digits) base

_foldToDecimalH digits base = foldl func (0, 1) digits
    where func (sum, currentBase) next = (sum + currentBase * next, currentBase * base)
