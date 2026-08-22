# Whatever-priming now stops at thunk barriers (ADR-0033 Phase 4)

`(1..10).grep(* > 3 && * < 8)` returned `5 6`. Rakudo returns `1 2 3 4 5 6 7`. No error,
no warning — just a quietly wrong list, in one of the most idiomatic shapes in Raku.

The cause was mutsu priming `*` straight *through* the thunky operators. Raku compiles
the operands of `&&`, `||`, `//`, `and`, `or`, `andthen`, `orelse`, `notandthen` and each
of the ternary's three parts as **thunks**, and Whatever-priming happens per thunk. So
`* > 3 && * < 8` is two independent arity-1 `WhateverCode`s; the `&&` then runs at its own
evaluation time, sees a truthy `Code` object on the left, and yields the right-hand
`WhateverCode`. mutsu instead built one arity-2 closure, so `grep` fed it pairs. The
ternary was the same bug from the other side: `Expr::Ternary` appeared in none of the
priming predicates, so mutsu primed *nothing* there and a bare `Whatever` survived to the
runtime, dying while coercing to `Numeric`.

Every row of the divergence table in
[ADR-0033](../../docs/adr/0033-whatever-priming-leaf-and-derived-scope.md) now matches the
system `raku`, and the new `t/whatever-thunky-operators.t` is dual-oracle: its 34
assertions pass verbatim under both mutsu and `raku`.

## The rule has two halves, and the second one is small

Section 4 of the ADR assumed this phase had to replace the parser's ~50 priming-scope
sites with one call. Measuring first showed otherwise. The fix decomposes as:

1. **A thunk barrier is opaque to the enclosing scope.** `contains_whatever`,
   `count_whatever` and both `replace_whatever_*` walkers stop dead at one — Rakudo's own
   model, in which a thunky op is never "whatever-ish". Every parser planting site is
   gated on `contains_whatever`, so *no site can any longer propose a scope that spans a
   barrier*. The fifty-site rewrite turned out to be unnecessary for the correctness goal
   rather than merely deferrable.
2. **Each barrier operand is a scope of its own.** The new `src/whatever_curry/plant.rs`
   owns exactly this, and it hooks into the walk `mark.rs` already performs — which the
   ADR's section 2.3 had predicted would happen ("deliberately the seed of the `plant.rs`
   section 4 calls for — same traversal, same parent-context switch").

Half 1 is why the awkward residue case needed no special case at all:
`((* > 3 && * < 8) + *)` is a single arity-1 `WhateverCode` in rakudo, and with the
barrier opaque the enclosing `+` simply sees one placeholder.

## The prerequisite, and a bug it flushed out

Making `&&` a barrier first required separating it from the `&&` the parser *synthesizes*
when it expands a chained comparison (`a < m < b` → `(a < m) && (m < b)`, middle
duplicated). The two must land on opposite sides of the rule:

```
(1 < * < 10)(0)        False   # one arity-1 curry over the whole chain
(1 < * && * < 10)(0)   True    # a real && yields only its right-hand thunk
```

A dedicated `TokenKind::ChainAnd` does that, keeping the `Expr::Binary` shape so no
expression walker needed changing (the ADR's alternative, a full `Expr::ChainedCompare`
node, needs an arm in every walker and fails *silently* where one has a catch-all; it is
worth doing for RakuAST rendering fidelity and is now filed as
`todo/tickets/chained-compare-ast-node.md`).

Separating them also fixed a real bug. The middle-duplication de-duplication in
`count_whatever` had to guess, by structural comparison, whether an `&&` came from a chain
— and it guessed wrong on a user-written `1 < * && * < 10`, collapsing it into one
arity-1 closure. With the synthesized conjunction now labelled, the heuristic fires only
where it belongs and that expression yields its right-hand thunk, as rakudo does.

## `xor` and `^^`

The ADR flagged `xor` as unresolved and asked for a measurement rather than a guess.
Rakudo primes neither `xor` nor `^^`: both `(* + 1 xor * + 2).WHAT` and
`(* + 1 ^^ * + 2).WHAT` are `Nil` with a "Useless use of `+` in sink context" warning.
Since neither barrier nor non-barrier treatment reproduces that, both stay off the barrier
list and mutsu keeps returning `(WhateverCode)` — recorded as a known divergence instead
of being guessed at.

Validated with the full `t/` suite (3361 files, 31562 assertions), `cargo test
--workspace`, and the whitelisted `roast/S02-*`/`S03-*`/`S04-*` sweep, all green.
