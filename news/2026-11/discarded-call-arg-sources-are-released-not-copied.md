# A discarded call-arg-source list is released instead of copied

`take_pending_call_arg_sources` hands the caller's argument-source names (`Vec<Option<String>>`)
to the binder, copying the whole list, every name included, whenever the call site's constant
still shares it. Four sites only wanted to *drop* the list (the light closure / light method
binds and the method dispatch gate) and paid that copy: one `Vec` and one `String` per named or
variable argument, per call. They now call `discard_pending_call_arg_sources`, a refcount
decrement. `pop_pair_operands_capturing` also stopped re-allocating the source variable's name for
every `x => $var` argument (the symbol's `&'static str` is borrowed).

`benchmarks/method-call.raku` (`Point.new(x => $_, y => $_ * 2)` x 10000, profiling build,
callgrind): 228,289 -> 198,289 allocations (-13%), 552.3M -> 545.5M instructions (-1.2%).
The pre-#16002 figure is 176.7k allocations: what is left is the readonly cell of a
non-variable named argument and the cell promotion of `x => $_`, the semantics #16002 added.
Refs #16098.
