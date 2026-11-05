# A method call no longer writes its plain scalar parameters into the frame env

The fast method path (`call_compiled_method_fast_in_unit`) already skipped the
fixed names, the callable id and attributes for a method that cannot write the
env, but it still inserted every bound parameter by name. A plain scalar
parameter (`method m($x)`) is read through its slot, so for a method with no
closures that insert was a dead write: 58% of the fast-path frames measured over
`t/oo` and `t/classes` ended with a non-empty overlay, and the parameters were
most of what was in it. The path now keeps the env entry only for the
parameters something else names: an aggregate (`@x`, `%x`, `&x`), an attribute
parameter, a named or slurpy one, a sigilless one, an invocant, and an
`is rw` / `is copy` / `is raw` one.

On a one-scalar-parameter method called 100,000 times, instructions went from
2,705.4M to 2,677.1M (about 280 per call, second callgrind run of each, the
same build otherwise). This is the first step of ADR-14633 slice 4: a frame
whose overlay stays empty is the precondition for not allocating it.
