# `unique` and `repeated` stop being quadratic

`(^160_000).unique` never finished under mutsu. rakudo answers it in 0.5s.

Both `unique` and `repeated` kept the values they had already seen in a plain
`Vec` and compared every candidate against all of them, so a list of `n`
distinct values cost `n^2/2` identity comparisons. The shape was visible in a
one-line measurement — 0.33s at 2000 elements, 1.3s at 4000, 5.2s at 8000 —
and at the 160_000 elements a real distribution asked for, it was simply a
hang.

That distribution is `MongoDB::Fast`, whose `t/08-request-id-unique.rakutest`
mints 160_000 request ids across 8 threads and asserts they are all distinct.
It is one of the 21 files in the ecosystem parity ledger's `timeout` cluster
([#7995](https://github.com/tokuhirom/mutsu/issues/7995)), and the previous
triage pass read it as evidence *against* the cluster's `todo:perf` framing:
the test passes its two expensive assertions and then appears to hang on a
trivial one, so "nothing about it is slow". The assertion it hangs on is
`@ids.unique.elems`, and it was slow — quadratically.

## The fix

`unique` and `repeated` now keep their seen values in an `IdentityIndex`
(`src/runtime/utils/identity_index.rs`), which hash-buckets them. Rakudo does
the same thing by keying on `.WHICH`; mutsu has no single hashable identity
string, so the index is deliberately weaker than a hash set: a bucket only
*narrows* the scan, and the full identity predicate still decides every
candidate the bucket produces. A hash collision therefore costs one comparison
and can never change an answer.

That leaves one invariant for the bucket function to uphold — two values that
compare identical must never land in *different* buckets — which is why a kind
with a cross-kind identity arm gets no bucket at all. `Package("int")` is
`values_identical` to `Int(0)`, and a `Mixin` carrying only the read-only topic
marker is transparent to its inner value, so both stay unbucketed, and every
unbucketed value is scanned by every candidate regardless of whether that
candidate is bucketed. `Int` and `BigInt` do compare across representations, so
they share one bucket space instead of being excluded. Only `Int`, `BigInt`,
`Str`, `Bool` and `Num` get a bucket today; everything else keeps the old full
scan and the old cost.

`:with` keeps the full scan too. A user comparator defines its own equality
class, which no index can model — rakudo's `:with` path is quadratic for the
same reason.

The change landed in both implementations, because there are two: the native
0-arg fast path in `builtins/methods_0arg/dispatch_core_list.rs` (which is the
one an `@array.unique` actually reaches, and which carried the loop three times
over, once each for Array, Seq and Slip) and the `runtime/` dispatch path that
handles the `:as` / `:with` adverbs. The three native copies collapsed into one
helper each. The two paths disagree on one edge — the dispatch path treats two
placeholder-id instances of the same class as distinct, the native path does
not — so the index takes the predicate as a parameter rather than quietly
picking one of them for the other.

## Result

| | before | after | rakudo |
|---|---|---|---|
| `(^8_000).unique` | 5.194s | 0.017s | 0.029s |
| `(^160_000).unique` | (never finished) | 0.372s | 0.515s |

(debug build, so the comparison against rakudo flatters rakudo.)

`MongoDB::Fast`'s `t/08-request-id-unique.rakutest` goes from a sweep timeout
producing zero assertions to 3/3 passing, moving the file from `regression` to
`parity` in the ledger.

`t/collections/transform/unique-repeated-scale.t` pins both halves: the answers
for every value kind (including the unbucketed ones and the cross-kind identity
arms the bucketing must not break), and that 20_000 distinct values complete at
all. All 27 of its assertions pass under rakudo too.
