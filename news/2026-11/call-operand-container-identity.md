# `=:=` compares the container a Block call returns

`f($a) =:= $b` for `my &f = { $_ }` was True whenever the values were equal,
because the call result and the variable were both read as values. A call
operand against a scalar variable (or another call) now compares the cells: the
variable is captured as its cell and the result of the call is kept raw, so
`f($a) =:= $a` is True and `f($a) =:= $b` / `g() =:= $a` for a `sub g` are False,
as in raku (#17134).
