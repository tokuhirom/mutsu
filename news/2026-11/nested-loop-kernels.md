# Loop kernels take nested loops and `.Num`

A loop kernel (ADR-19288 stage 2) now lowers a counted `for` or a `while`
nested in its region as a native loop in place, and a `.Num` call on an `Int`
or `Num` as a conversion. So `benchmarks/bench-mandelbrot.raku`, whose outer
`for` computes `$py.Num * 0.05e0 - 1.5e0` and holds a second `for` and the
escape-time `while`, now runs as one kernel entered once, instead of an
interpreted pair of `for` loops entering a `while` kernel 3,585 times.

A `.Num` site is vouched for at kernel entry by the method-site lane, the same
gate an ordinary `$x.Num` takes: it must answer a sample receiver with the
built-in method's `Num` (an augmented, wrapped or `find_method`-intercepted
method is refused, and the loop stays generic). A bail inside a nested loop
reruns the enclosing kernel iteration from its start, so an inner overflow
continues the whole outer iteration generically.

`bench-mandelbrot`, release build: 494.8M → 14.4M instructions with kernels on
(68.2M before this change), 38.9 ms → 8.0 ms wall against rakupp v5.9.0's
7.3 ms (minimum of 7 runs on the 4-core container).
