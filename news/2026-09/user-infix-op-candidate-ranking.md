# A user `multi infix:<+>` no longer beats the core operator for every type

`multi infix:<+>($a, $b) is default { "USER" }; say 1 + 2` printed `USER`. In
rakudo it prints `3`: an operator is a `multi`, so a user candidate *joins*
`&infix:<+>`'s candidate set instead of replacing the operator, and an untyped
parameter is `Any` — wider than the core `(Int:D, Int:D)` candidate, which
therefore takes the call.

This is the infix half of
[ADR-0071](../../docs/adr/0071-native-operators-are-dispatch-candidates.md),
whose increment half (`prefix:<++>` and friends) landed the same day. mutsu
implements the operator natively, so there was no candidate for the user's
`($a, $b)` to lose a narrowness comparison against: `try_user_infix`
(`src/vm/vm_arith_ops.rs`) handed *every* matching user candidate the call.

`src/runtime/native_infix_dispatch.rs` supplies the missing half. It models each
natively-implemented infix's core candidate set as the pairs of type constraints
its two-operand candidates bind — transcribed from rakudo's own
`&infix:<op>.candidates` — picks the narrowest one that accepts the operands, and
ranks it against the user's candidate with the metrics ordinary multi dispatch
already uses (`candidate_specificity_rank_for_args` +
`candidate_type_distance`). Ties go to core, as in ADR-0071. When core wins,
`try_user_infix` reports "no user candidate" and the native path runs exactly as
before.

## Why a vendored table rather than promotion tiers

The ticket left the modelling open: derive the sets from rakudo once and vendor
them, or model a small set of numeric-promotion tiers generically. Measurement
decided it. A tier model has to answer "what is the narrowest core constraint for
this operand", and for `1 + 2e0` it would answer `(Int, Num)` at distance 0 — but
rakudo has **no** `(Int:D, Num:D)` candidate, so the call binds `(Real, Real)`
and a user `multi infix:<+>(Int $a, Num $b)` *wins* (measured). The fact being
used there is the *absence* of a pair, which no per-operand tier model can see.

The table turned out far cheaper than the ticket feared, because operators share
their sets: ~60 rows in nine groups cover 22 operators. An operator with no entry
keeps the old behaviour (the user candidate wins), which is right for a purely
user-defined `infix:<@@>`.

## The hot path is untouched

`try_user_infix` is on the arithmetic hot path, but it already bails on
`user_declared_infix_ops.is_empty()` before doing anything, so the ranking is
unreachable unless a user `infix:<op>` of that name is in scope. `rust-gdb
-batch` breakpoints on `try_user_infix`'s post-guard body and on
`core_infix_candidate_wins`, running `benchmarks/int-arith.raku`, hit **zero
times**; the same breakpoint fires on the first `+` of a program that declares
`multi infix:<+>`, so the probe is valid.

## Four dispatch gaps fixed underneath

Each was a pre-existing wrong answer for ordinary `multi` dispatch, exposed the
moment an operator had a core candidate to lose to:

- `type_hierarchy_distance` scored the `Rational` role as `UNRELATED` even though
  `(1/2) ~~ Rational` is True, so rakudo's `(Rational:D, Rational:D)` row could
  not be expressed.
- An enum value ranked as its base type: `multi f(A $x)` lost to
  `multi f(Int $x)` for `enum A <e1 e2>; f(e1)`. An enum value now narrows in
  three steps — its own value name, its enum type, then the base type.
- An enum *value* used as a parameter (`multi infix:<->(e1, e2)`, roast
  `S03-operators/custom.t`) scored `UNRELATED`; it is the narrowest constraint
  there is.
- `UInt` did not rank as a `subset` (it is `subset UInt of Int where * >= 0` in
  rakudo but a type-matching special case in mutsu), so `multi f(UInt $x)` lost
  every tie to `multi f(Int $x)` for `f(10)`.

`exec_mod_op` was also the one arithmetic opcode that never consulted a user
infix at all, so `multi infix:<%>(P $a, P $b)` was unreachable for
`P.new % P.new`. It goes through `try_user_infix` now, like `+`/`-`/`*`.

## Pin

`t/user-infix-op-candidate-ranking.t`, 49 rows each in its own `EVAL`, passing
identically under `raku` and under `mutsu`. Two rows are `todo`-marked: rakudo
rejects an exact nominal tie (`multi infix:<+>(Int $a, Int $b)` for `1 + 2`) with
`X::Multi::Ambiguous`, while mutsu gives the tie to core and runs the operator
silently — raising the error needs the core candidate to carry a renderable
signature, which is ADR-0071's still-deferred alternative B.
