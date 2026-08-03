# A `start` block read the wrong destructured `@` parameter, and `|$buf` did not slip

Two more general bugs from grondilu's `Digest::RIPEMD`, which now computes the
correct RIPEMD-160 digest for every RFC test vector.

## 1. A destructured `@`/`%` parameter was frozen at the first spawn's value

```raku
await map -> [$a, @K] { start { "$a:{@K[0]}" } }, (1, (100,101)), (2, (200,201))
# was  1:100, 2:100
# now  1:100, 2:200
```

`@`/`%` lexicals captured by a spawned block are carried by the process-wide,
bare-name-keyed `shared_vars` store rather than by the closure machinery
(`docs/recursive-start-shared-vars.md` — `$` names were moved off that lane and
given a per-binding home; aggregates were deliberately left on it, because the
`__mutsu_atomic_*` CAS copies are keyed off those entries). That store is seeded
once per name with `seed_if_absent`, so the first spawn's value is immortal, and
the worker prefers it over its own env copy.

A destructuring sub-signature is the one parameter path that writes `env` with no
local slot behind it — every compiled binding path declines a `sub_signature`, so
`bind_sub_signature_from_value` is the whole story — and what it binds is
provably a *fresh per-invocation binding*, never the one shared object the lane
exists to represent. Those names are now recorded as they are bound
(`Interpreter::sub_signature_bound_aggregates`) and, at spawn time, kept off the
lane and masked in the child, exactly as a captured `$` already was. Two
conditions narrow it to that case: the name must be one a sub-signature bound,
*and* the spawned block's free variable must resolve to no parent slot. A plain
`-> @K` parameter therefore keeps the lane, as do unrelated outer aggregates that
merely share a name.

The general form of this — two ordinary bindings of one `@` name, one of them
captured by a spawn — is not fixed here; it needs the per-binding home the doc
defers. Recorded as `todo/tickets/shared-var-lane-freezes-a-reused-array-name.md`.

Pinned by `t/start-block-destructured-array-param.t`.

## 2. `|$buf` slipped the buffer, not its elements

A `Buf`/`Blob` is `Positional`, so `|$buf` slips its elements — and at the
buffer's own element width, so `|blob32.new(7, 8)` is `slip(7, 8)`.
`exec_make_slip_op` had no arm for a buffer instance, so it fell through to
`_ => vec![val]` and produced a one-item slip holding the whole buffer:

```raku
my $b = blob32.new(7, 8);
say (1, |$b, 2);   # was (1, Blob[uint32].new(7,8), 2), now (1, 7, 8, 2)
```

`Digest::RIPEMD` renders its digest with `map |*.polymod(256 xx 3), |$reduced`,
where `$reduced` is a `blob32` — so the `WhateverCode` was handed the whole Blob
and digested a numified `0`, giving four zero bytes for every input. A type
object (`|Buf`) carries no element storage and stays one item, as in Rakudo.

Pinned by `t/slip-a-buf.t`.

## What is left in `Digest::RIPEMD`

`rmd160` now returns the correct digest for every RFC vector — but only for the
*first* call in a process. Its output stage rotates the five hash words with
`map { $_[[^5].rotate(++$)] }`, and mutsu never resets an anonymous `$` state
variable when its enclosing routine is re-entered, so later calls rotate by the
wrong amount. That is an independent bug with its own minimal repro, recorded as
`todo/tickets/anonymous-state-var-not-reset-per-routine-call.md`.
