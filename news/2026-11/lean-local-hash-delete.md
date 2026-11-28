# A lean `%h{$k}:delete` for a plain declared `%` local

`%h{"key-$i"}:delete` cost about 7,800 instructions. Before
`try_fast_hash_delete` even resolved the variable by name in env, the delete
op passed through all of these:

- an attribute-element snapshot and the package-body lexical seed;
- a `%*ENV` publish check and a user `postcircumfix` probe;
- the ADR-0068 structure guard and a key-sequence normalization;
- the delete-home entry and a shaped-hash probe;
- the `is Hash` / user-object delegation checks.

`DeleteIndexNamed` carries the base container's compile-time slot. When the
running chunk declares it as a plain user `%` lexical, the delete now takes
the lean element store's checks (#19306): the chunk's slot class, the process
latches, the variable's flags and the hash node's metadata. It then removes a
`Str` or `Int` key with one probe. It answers what `try_fast_hash_delete`
answers: the removed entry as stored, or `Any` for an absent key. A bind
token or Proxy entry, a typed or defaulted hash, a slice, or a thread all take
the general delete as before.

callgrind, profiling build, JIT on, `BENCH_DET=1`:

| file | before | after |
|---|---:|---:|
| `benchmarks/bench-hash.raku` | 177,518,155 Ir, 133,075 allocations | 141,653,370 Ir, 103,074 allocations (-20%) |
| 20,000 stores then 20,000 deletes in a sub | 209,261,699 Ir, 144,463 allocations | 84,233,709 Ir, 24,464 allocations |
