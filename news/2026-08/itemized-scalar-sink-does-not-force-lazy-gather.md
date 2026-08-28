# A scalar-itemized lazy `gather`/`Seq` no longer force-reifies on a later discarded read

`mutsu`'s sink-context forcing — the machinery behind `OpCode::SinkPop`,
`OpCode::SinkPopAssign`, and the statement-level-call sink path in
`vm_call_exec_ops.rs::sink_discarded_call_value` — ran a not-yet-touched
lazy `gather`/`Seq` for side effects whenever its value was discarded, with
no distinction for whether that value had already been assigned to a plain
scalar variable. Raku's own rule is narrower: discarding a genuinely bare
(un-itemized) lazy value in sink context runs it for side effects (that is
why `(1..3).map({ .say })` as a bare statement prints), but discarding an
**itemized** value — one that was assigned into a `$`-sigiled scalar,
`my $s = SEQ` or `$s = SEQ` — is a pure no-op (only "Useless use of $s in
sink context"), even when that itemized value later flows back out through
a routine or closure's own return and the caller discards *that*.

The minimal repro from `roast/S02-types/array.t`'s `zen and whatever slices`
subtest:

```raku
sub call-it(&c) { my $ok = 1; c(); CATCH { default { $ok = 0 } }; $ok }
say call-it({ my $s = (gather die)[] });
```

Raku prints `1` — the zen slice never forces the `gather`, so `die` never
runs. mutsu printed `Nil`: the closure's return value ($s's itemized
`gather`) got force-reified once `c()`'s result was discarded by `call-it`,
raising `die`, and the resulting signal was mishandled (see the companion
entry `gather-lazy-force-signal-delivery.md` for that half). Without any
`CATCH` at all, mutsu simply died where raku lived.

## Fix

Both of mutsu's lazy-sequence representations now carry a persistent
"itemized" flag, set exactly once at the point of a plain (non-bind, non-`@`/
`%`) scalar assignment and consulted by every sink-forcing site:

- `SeqBody` (the `Seq`/`ADR-0034` representation, `src/value/seq_body.rs`)
  gained an `itemized: bool` field alongside the existing `cache_requested`/
  `retained` state and a `mark_itemized()` setter (mutated in place through
  the shared `Arc`, exactly like `mark_cache_requested`). `SeqBody::sink`
  now exempts an itemized body the same way it already exempts a
  cache-requested or retained one.
- `LazyList` (the still-tree-walk-flavoured `gather`/`take` coroutine
  representation, `src/value/mod.rs`/`value_lazy.rs`/`value_lazy_ctors.rs`)
  gained the analogous `itemized` field, `is_itemized()`/`with_itemized()`
  accessors (the value-clone pattern already used for `cached_no_sink`,
  since `LazyList` has no single shared mutable core the way `SeqBody`
  does), and the two sink-forcing call sites (`OpCode::SinkPop` in
  `vm_exec_dispatch.rs`, and `sink_discarded_call_value` in
  `vm_call_exec_ops.rs`) now check `is_itemized()` alongside
  `is_cached_no_sink()`.
- `src/vm/vm_var_assign_set_local.rs`'s `SetLocal` handler — the single
  chokepoint for both `my $x = ...` declarations and plain `$x = ...`
  reassignment — marks a `Seq`/`LazyList` RHS itemized when the target name
  is `$`-sigiled (mirroring the pre-existing `@`/`%` branch there, which
  instead eagerly reifies for array/hash-flattening semantics).

Consuming the itemized value later (`.raku`, `.Str`, iteration, ...) still
forces it exactly as before — itemization defers forcing, it does not cancel
it. A genuinely bare (never-assigned) lazy value still forces on sink, same
as always; only a value that passed through a scalar container is exempt.

Pin: `t/itemized-scalar-sink-does-not-force-lazy.t` (8 assertions, all green
under `raku` too).

## Impact

Closes `roast/S02-types/array.t`'s `zen and whatever slices` subtest under
`MUTSU_REAL_TEST=1` (the real vendored `Test.rakumod`), and fixes a general
correctness gap independent of `Test`: any real-world script that assigns a
lazy `gather`/`Seq` to a scalar and later discards a read of it in sink
context — including through an intervening routine call — previously risked
eagerly running side effects (or dying) that raku defers forever.
