# A `for` loop over a list of plain `Int`s runs as a loop kernel

ADR-19288's loop kernels ran a counted `for` over an `Int` range, or a
`while`, as one native loop. A `for` over a list it already holds, such as
`for %h.values { $sum += $_ }`, still went through the general body driver:
per item it bound the topic, checked the aliasing and writeback plans, and
entered the JIT body, whose `$sum += $_` stepped `CheckReadOnlyModify` and
`SetLocal` through their generic shims -- about 1,800 instructions per item.

Such a loop now runs as the same closed `Int`/`Num` kernel when it reads its
topic only through its own slot (the topic-slot change of #19416), nothing
else is bound per item, and every item is a plain `Int`. The kernel's loop
variable is the item at the counter, loaded from a copy of the list
(`LoopShape::Range { list: true }`); the counted-loop kernel is unchanged.
A bail (an overflowing `+`, a zero divisor) hands back the index of the
item it could not finish, and the generic driver resumes there.

callgrind, profiling build, JIT on (on top of #19422):

| measurement | before | after |
|---|---:|---:|
| `benchmarks/hash-access.raku`, `BENCH_DET=1` | 73,943,990 Ir | 54,151,036 Ir |
| `for %h.values { $sum += $_ }`, per item | 1,808 Ir | 211 Ir |

A `for @array { ... }` loop reads the array live and does not take this
path yet.
