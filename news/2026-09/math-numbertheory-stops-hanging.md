# Math::NumberTheory stops hanging, and three general bugs come out of it

`Math::NumberTheory`'s `t/01-integer-factors.rakutest` was one of the 17
never-finishing members of the ecosystem timeout cluster ([#7995](https://github.com/tokuhirom/mutsu/issues/7995)):
it did not complete in 300s, against rakudo's 9.97s. It now runs in **0.67s
with all 12 subtests passing**. None of the three bugs behind it was specific
to that distribution.

## `classify(*)` / `categorize(*)` keyed everything under `Nil`

`multi method classify(Whatever)` classifies on the *identity* of each element
(documented under `Type/Any`; rakudo added it in 2023.02). mutsu's classifier
matched the mapper against `Sub`, `Hash` and `Array` and answered `Nil` for
anything else, so `*` produced a single `Nil` bucket holding every element.
`factor-integer` uses `@primes.classify(*)` to turn a prime-factor list into
`(prime, multiplicity)` pairs, so `factor-integer(120)` answered
`[(Nil, 5)]` where rakudo answers `[(2, 3), (3, 1), (5, 1)]`.

Identity behaves exactly like a `{ $_ }` block mapper, so the one-line arm
that hands the element back gets the rest for free: a list-valued element
becomes a multi-level path for `classify` (mixed levels still raise
`X::Invalid::ComputedValue`) and several categories for `categorize`, `:as`
still decides the stored value while the key stays the element, and a `Bag`
or `Set` receiver keys on its pairs.

## A `VarRef` argument reached the pure native function table

This is what made the file *hang* rather than merely answer wrongly.

A plain-variable argument reaches a call site wrapped in a `VarRef`, so an
`is rw` parameter can bind the caller's container. `normalize_call_args_for_target`
strips the wrapper for an unregistered name but deliberately keeps it when a
user routine of the same name is registered. Nothing in `builtins/functions.rs`
knows the wrapper — every handler there is pure Rust over values — so a
`VarRef` that survived fell into the handler's catch-all arm. Declaring *any*
`multi sub abs(...)` was enough to make `abs($n)` answer `0`, and any
`multi sub is-prime(...)` enough to make `is-prime($n)` answer `False`, while
the literal spellings `abs(-3)` and `is-prime(3)` stayed correct.

`Math::NumberTheory` declares `multi sub is-prime(Complex:D)` to extend the
routine to Gaussian integers. Its own trial division then saw `is-prime($d)`
answer `False` for every divisor, divided out nothing, and walked `$d` toward
`sqrt(20!)`.

The wrapper is now stripped at the single door into the table rather than at
each of its callers, behind a tag probe that costs nothing when no argument
carries one.

## The word-named infix operators ignored user candidates

[ADR-0071](../../docs/adr/0071-native-operators-are-dispatch-candidates.md)
made mutsu's native operator implementations *candidates* of `&infix:<op>`, so
a user `multi` that out-narrows the core set takes the call. `div`, `mod`,
`gcd`, `lcm`, `min` and `max` were never wired into it: their opcode handlers
computed the integer answer directly, which made a user candidate for them
unreachable. `Math::NumberTheory` overloads `gcd`/`lcm` for Gaussian integers
and Rationals, so `(10 + 15i) gcd 25` answered `25` instead of `0-5i`, and
`(1/3) gcd (2/5)` answered `0` instead of `1/15`.

All six now consult `try_user_infix` first, and `gcd`/`lcm`/`min`/`max` gained
core candidate shapes so an *untyped* user candidate still loses to the
narrower core one — `12 gcd 18` is `6` whether or not a
`multi infix:<gcd>($a, $b)` is in scope.

Pinned by `t/oo/class/classify-whatever-identity.t`,
`t/routines/dispatch/builtin-shadow-varref-dispatch.t` and
`t/lang/operators/infix-word-name-overloading.t`.
