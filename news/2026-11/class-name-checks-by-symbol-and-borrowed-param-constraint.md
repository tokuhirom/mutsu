# Binding a typed method parameter no longer copies its constraint

The fast method binder built an owned `String` for every typed parameter's
resolved constraint (`method distance-to(Point $other)`), and the BOOT scalar
and `Failure` class-name probes that every bind and definedness test runs
compared names through `Symbol::as_str` plus string equality. The constraint now
borrows when nothing resolves it, and the class-name probes compare symbols.

`benchmarks/method-call.raku` (callgrind, profiling build, second run, same
base): 557,978,139 to 553,247,320 instructions (-0.85%), 198,421 to 188,424
allocations (-5%). `Refs #16098`: the named-argument cells are still to do.
