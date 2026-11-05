# `Date.new(2022, 1|2, 5)` autothreads over a Junction argument

A Junction positional argument of the native `Date.new` was coerced (month `0`, "Month out of
range") instead of threading the call. Core classes whose `new` declares typed positional
parameters (`Date`, `DateTime`, `Rat`, `FatRat`, `Complex`, `Version`, `Duration`, `Num`, `Int`) now
thread a Junction argument like any such method, so `Date.new(2022, 1|2, 5)` is
`any(Date.new(2022,1,5), Date.new(2022,2,5))`. The collection and default constructors still store
the Junction (`Set.new(1|2)`, `Array.new(1|2)`). Closes #16675.
