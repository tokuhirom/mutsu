# A method call's named arguments are checked for laziness by their tags

Before a user method binds its arguments, the VM reads any deferred
`.map`/`.grep`/gather Seq a named argument carries
(`reify_map_grep_seq_args`), so `Any.new(:specs(@x.map: {...}))` fills its
attribute. To look, it copied every named argument's value out of its
container and ran three no-op checks on the copy: about 400 instructions per
argument, on every argument of every method call, almost none of which is
lazy.

It now asks the value's tags through its container first and skips the copy
and the checks unless the value is a Seq or a lazy list.

`benchmarks/bench-ctor.raku` (one run, `BENCH_DET=1`, callgrind on the
profiling build): `reify_map_grep_seq_method_args` went from 11.6M to 5.1M
instructions, the whole run from 443.9M to 437.0M. Part of #19399.
