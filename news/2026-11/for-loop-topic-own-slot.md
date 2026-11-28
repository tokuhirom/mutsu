# A `for` loop that cannot observe its topic reads `$_` from its own slot

A `for` block without parameters binds `$_` per item. mutsu kept that topic
in the environment: the loop driver rebound the env's `_` on every iteration,
and each `$_` in the body was a by-name `GetGlobal("_")` that walked the
environment and, under the JIT, left native code to step the interpreter.
In `for ^N { %h{"key-$_"} = $_ * 2 }` that was two name lookups and one env
rebind per iteration, about 1,200 instructions.

When the body passes the existing allow-list that already proves the topic's
container unobservable (#14495: no call into user code, no closure, no
write, bind or capture of `$_`), the compiler now gives the loop a hidden
local slot of its own for `$_`. The body reads it with `GetLocal`, which the
JIT compiles natively, and the VM writes only that slot per item
(`ForLoopSpec::topic_slot_only`); the env keeps the enclosing topic, which is
what a `$_` after the loop reads anyway. Source-variable writeback is skipped
for such a loop, since its body cannot write `$_` and the env's `_` it would
read is the enclosing topic.

callgrind, profiling build, JIT on:

| measurement | before | after |
|---|---:|---:|
| `benchmarks/hash-access.raku`, `BENCH_DET=1` | 94,115,457 Ir | 76,664,973 Ir |
| store loop `%h{"key-$_"} = $_ * 2`, per iteration | 4,939 Ir | 3,751 Ir |
| whole workload (store + `.values` sum), per element | 7,507 Ir | 5,567 Ir |
