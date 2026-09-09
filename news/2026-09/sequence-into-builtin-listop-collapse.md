# A `...` sequence passed straight to a builtin listop no longer collapses

`(SEED, *code ... ENDPOINT)` handed directly to a builtin listop (`join`,
`sum`, `min`, `max`, `sort`, and transitively `grep`/`map`) used to silently
answer wrong: `join` saw only the seed, `sum` answered `0`, `min`/`max`
returned the whole sequence as one candidate, and `sort` either refused it
as "lazy" or dropped it entirely. The sequence's own value was always
correct — `say (...)`, `.join`, `elems (...)`, and binding into a user
`*@`-slurpy all worked — only a *pure* native listop reading the raw value
was affected.

Root cause: `eval_sequence` defers a closure (`WhateverCode`-generated)
`...` sequence whose value endpoint it has not yet reached to a `LazyList`
(`src/runtime/sequence.rs`'s "Defer an unfinished finite closure sequence"
comment) — reaching a value endpoint needs incremental generator evaluation,
so the sequence keeps only its eager prefix cached and lets a later forced
read finish the job. The pre-dispatch pass that reifies a still-deferred
`.map`/`.grep` Seq before handing arguments to a pure Rust native function
(ADR-0058, `reify_map_grep_seq_args`) knew nothing about this different
lazy flavour, so `join_flat`/`sum`/`min`/`max`/`sort`'s native
implementations read the LazyList's partial (or, for `join`, single-item)
cache as if it were the finished answer.

Fixed with a new `Interpreter::reify_closure_seq_endpoint`
(`src/vm/vm_helpers_lazy.rs`), called alongside the existing map/grep
reification in `reify_map_grep_seq_args`: it forces exactly a `LazyList`
with `has_finite_closure_endpoint()` — a closure sequence whose endpoint is
a concrete value, so forcing it to completion can never hang, unlike a
genuinely infinite `... *`. Three call sites also needed a `LazyList` arm of
their own (mirroring their existing `Array`/`Seq` arms) to actually read the
now-complete cache instead of treating an unrecognized `LazyList` as a
single opaque value: `sum`'s variadic native dispatch
(`dispatch_variadic.rs`), the shared `min`/`max` flatten step
(`extrema_from_values_generic`), and `sort`'s both native
(`dispatch_1arg.rs`) and interpreter (`builtin_sort`) paths. `sort` also had
an over-eager `is_lazy_for_coerce` guard (`methods_collection.rs`) that
treated every `LazyList` as unsortable regardless of whether it was
genuinely lazy; narrowed to `ll.is_genuinely_lazy()` so a finite,
already-reified closure sequence is eligible for sort/classify/QuantHash
coercion like an eager Seq, while an actually-infinite source still throws
`X::Cannot::Lazy`.

`join` needed no code change beyond the shared reification pass — its
`join_flat`/`flat_val` helpers already read a `LazyList`'s cache correctly
once it is fully populated; they were just never given a fully-populated
one before.

Verified against `raku`: the ticket's three repro lines, every control that
was already correct, a sweep across `grep`/`map`/`min`/`max`/`sort`, and the
lazy-safety controls (an infinite range/sequence stays lazy, and a
genuinely-infinite `sort` still throws `X::Cannot::Lazy`) all match. Pinned
by `t/issue-7753-seq-into-builtin-listop.t`.

Closes #7753.
