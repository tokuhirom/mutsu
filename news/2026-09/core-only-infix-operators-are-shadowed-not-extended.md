# A user `multi infix:<cross>` now shadows the core `cross`, instead of extending it

ADR-0071 taught mutsu that a natively implemented operator is a *dispatch
candidate*, not a fallback: declaring `multi infix:<+>(Q, Q)` joins
`&infix:<+>`'s candidate set, so `1 + 2` keeps printing `3` instead of reaching
the user's catch-all. That was measured against rakudo and it is right — for the
operators rakudo actually declares.

It said nothing about the operators rakudo does *not* declare, and mutsu
extended those too. `infix:<cross>` is the case that surfaced it. rakudo has a
core `sub cross(...)` list operator but no `&infix:<cross>` whatsoever —
`raku -e 'say (1,2) cross (3,4)'` is "Two terms in a row". mutsu spells `cross`
as an infix as a convenience, reaching that core `sub` through the *bare-name*
last resort in `call_infix_fallback`. So a user declaration, which in rakudo
installs a fresh lexical routine with nothing behind it, kept the builtin alive
underneath in mutsu:

```raku
class Q {}
multi infix:<cross>(Q $a, Q $b) { "QC" }
say (1,2) cross (3,4);
```

```
raku : Cannot resolve caller infix:<cross>(List:D, List:D); none of these signatures matches:
           (Q $a, Q $b)
mutsu: ((1 3) (1 4) (2 3) (2 4))
```

## The classification, vendored rather than reasoned out

The fix is [ADR-0093](../../docs/adr/0093-core-only-infix-operators-are-shadowed-not-extended.md):
"does this operator have a core candidate set at all" becomes one question with
one answer, `CoreInfixCandidates`, which the ranking and the fallback both
consult. It has three cases — the operator is `Modelled` (rakudo declares it and
mutsu has a type table for it), `Unmodelled` (rakudo declares it, mutsu does not
model its candidates, so the native implementation still answers a call no user
candidate accepts), or `Shadowing` (rakudo has no such routine, so a user
declaration replaces it outright and a non-matching call is `X::Multi::NoMatch`).

The interesting part is how the third case is decided. Guessing which spellings
mutsu adds on its own would have been a standing source of wrong answers in both
directions, so the table is measured instead of reasoned: `core_infix_names.rs`
vendors all 143 `&infix:<...>` keys of rakudo's `CORE::` package, taken straight
from `CORE::.keys.grep(*.starts-with("&infix:"))` on rakudo 2026.07, sorted for
binary search. A name absent from it is `Shadowing` — which is exact by
construction, and covers `cross`, `zip`, `roundrobin`, mutsu's own extra
spellings (`sum`, `flat`, `unique`, `squish`) and every purely user-defined
operator (`infix:<@@>`) with no further enumeration.

The blanket version of the rule — "unmodelled means shadowed" — was tried first
and is wrong by a factor of six: only 22 operators are modelled where rakudo
declares 143, so `minmax`, `min`, `max`, `eqv`, `x`, `cmp`, `Z`, `..`, `but` and
the whole set-operator family would have started raising `X::Multi::NoMatch` for
calls rakudo answers with a core candidate.

The guard sits *after* every operator-named path has declined — `try_user_infix`,
the chain-op loop, `call_user_routine_direct(infix:<op>)` and the
`apply_reduction_op` reduction all run first and are untouched. Only the
bare-name fallbacks below them are shadowed.

Note that this is the same rule ADR-0044 already applies to listops, seen from
the other side: a user `multi splice` genuinely *does* extend the core, because
rakudo genuinely does declare `sub splice` as a multi. The question was never
"operator vs. listop" — it is, and always was, "does rakudo declare a routine of
**this** name".

## What it fixes

`Math::Vector` 0.6.0's `t/01-basics.rakutest` declares a dimension-guarded
`multi infix:<cross>` and then asserts that mismatched-dimension calls die.
Under mutsu those `dies-ok` calls fell through to the core list operator and got
a `Seq` back. The suite goes from **197/201 to 201/201**, matching its rakudo
baseline exactly.

`t/routines/dispatch/user-infix-op-core-candidate-set.t` pins the behaviour,
22/22 identically under `raku` and under `mutsu`: the three core-only names
shadow (and their own candidates stay reachable), the error message matches
rakudo line for line, `X` stays a separate name with its own core candidates,
and nine operators rakudo really does declare keep answering from core.

On the way, the four hand-copied `X::Multi::NoMatch` construction blocks (two in
`builtins_operators_fallback.rs`, one each in `calls.rs` and
`dispatch_proto_call.rs`) collapsed into one `Interpreter::multi_no_match_error`,
which the new fallback site reuses — so an operator's no-match error is the same
shape as every other routine's for free.

mutsu's own leniency is untouched: `(1,2) cross (3,4)` with nothing declared
still works, even though rakudo rejects it at compile time. Making the
speculative word-infix layer strict is a much wider change and is recorded as
the remaining divergence in the ADR.

Closes [#8006](https://github.com/tokuhirom/mutsu/issues/8006).
