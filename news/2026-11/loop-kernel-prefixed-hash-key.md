# A loop kernel stores under a `"prefix$i"` hash key

ADR-19288's loop kernels already ran `%h{$i} = $v` with an `Int` key
natively (#19380). The common `%h{"key-$i"} = $v` declined the whole loop:
the key is an interpolation, a `Str`, and a kernel holds only machine words.
Each iteration then ran through the interpreter's loop driver, the JIT body,
the interpolation (a `String` buffer and a `Str` box) and the lean element
store with its per-store checks -- about 2,900 instructions.

The kernel's typing pass now accepts the interpolation of a `Str` constant
and an `Int` when its only use is a hash store's key: the pieces are typed
`K::StrConst` and `K::IntKey`, which nothing else may consume and which may
not cross a join. The store calls `kernel_hash_store_prefixed_int` (or
`_num`), which builds the key's `String` from the constant and the `Int`'s
digits (`IntDecimal`) directly -- no `Str` value is made -- and files it
under the same checks as the `Int`-keyed store; a bound or proxied entry
declines and the iteration reruns generically. The text is the one the
interpolation produces: the constant is already NFC and a digit composes with
nothing. The kernel entry declines when a user `^find_method` exists, as the
interpreter's plain-piece interpolation does, and kernels never run with a
user `infix:<~>` declared.

callgrind, profiling build, JIT on (on top of #19436):

| measurement | before | after |
|---|---:|---:|
| `benchmarks/hash-access.raku`, `BENCH_DET=1` | 49,323,524 Ir | 29,061,840 Ir |
| `for ^N { %h{"key-$_"} = $_ * 2 }`, per iteration | 2,870 Ir | 1,091 Ir |

With #19416, #19422, #19426, #19431 and #19436, hash-access went from
94,115,457 Ir (main 48053d93) to 29,061,840 Ir. Its warm section measured
about 1.5 ms locally against rakudo 2026.11's ~5.5 ms and Raku++ v5.9.0's
~3.3 ms on the same box.
