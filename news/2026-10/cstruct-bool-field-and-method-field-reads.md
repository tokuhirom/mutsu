# CStruct: `bool` fields keep `True`, and methods see accessor writes

`has bool $.b` of an `is repr('CStruct')` class stored a zero byte when given `True` (`to_int` numifies a
`Bool` to 0), and the compiled-method fast path read the stale attribute cell instead of re-reading C memory,
so `$obj.top = 7` or `$obj.top += 5` was invisible to `$!top` inside the class's methods (CSS::Box's
`Rect.enclose`). Both are fixed: the CStruct fields are re-read before the fast-path decision and a `Bool`
is written as 0/1. Part of #14353.
