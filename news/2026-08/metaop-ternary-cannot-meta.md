# `Z??`/`X??`/`R??`/`S??` diagnose `X::Syntax::CannotMeta`, unblocking `??` in `duplicated_prefix_run`

`say ??1` now raises `X::Syntax::DuplicatedPrefix` ("Expected a term, but
found either infix ?? or redundant prefix ?"), the same way mutsu already
diagnosed doubled `^^`/`~~` (`news/2026-08/metaop-doubled-infix-base.md`).
Before this, `say ??1` raised `X::Syntax::Confused` and `my $x = ??1` raised
`X::Syntax::Malformed`, both via a generic parse-failure path rather than the
named exception rakudo gives.

## The naive fix, and what it actually broke

A first attempt (2026-08-06, `todo/tickets/duplicated-prefix-question-mark.md`)
just added `?` to `duplicated_prefix_run` and reverted: it regressed
`roast/S03-operators/ternary.t`'s "fiddly meta error indicates what operator
is used" subtest, which asserts that `Z??`/`X??`/`R??`/`S??` (an attempted
meta-op over the ternary) raise `X::Syntax::CannotMeta`, not
`X::Syntax::DuplicatedPrefix`.

Root-causing *why* required reproducing the test's exact call shape —
`throws-like "1 $op?? 2 !! 3", X::Syntax::CannotMeta, :operator{...}` — rather
than a bare top-level `EVAL`. That surfaced a detail the original ticket had
guessed wrong about: `throws-like` in mutsu is a **native** Rust
implementation (`src/runtime/test_functions/throws_like.rs`), not the
vendored `Test.rakumod`'s sorrows-searching fallback the ticket assumed. Its
own type-matching logic has a much simpler (and looser) fallback: when the
*expected* type name starts with `"X::Syntax"` and no structured exception was
attached, ANY error whose message contains the literal text `"parse error"`
counts as a match. mutsu's generic "Confused." parse failure always includes
that phrase, so `1 Z?? 2 !! 3` — which failed to parse at all, for entirely
unrelated reasons, as `X::Syntax::Confused` — satisfied the `CannotMeta`
matcher by accident. Adding `?` to `duplicated_prefix_run` changed the
*message text* for the doubled-`?` case, breaking that accidental match
without providing a real one.

## The fix: give `Z??`/`X??`/`R??`/`S??` a real, typed diagnosis

`cannot_meta_ternary_error` (`src/parser/expr/precedence_meta_ops/meta_bracket.rs`)
recognises the four adjacent spellings and builds a genuine
`X::Syntax::CannotMeta` instance carrying rakudo's own `meta`/`operator`/
`reason`/`dba` attributes, e.g. "Cannot zip with ?? 2 !! because conditional
operators are too fiddly" for `Z??`. It is checked at the top of the
list-infix loop (`src/parser/expr/precedence/list_infix_loop.rs`) — the same
place that already special-cased `Z.`/`X.` misuse — which runs *before* `Z`/`X`
ever fall back to their bare-infix reading. `R` and `S` have no other meaning
as meta-op letters (bare `R`/`S` aren't valid metaops at all); the check fires
purely on the literal adjacency, matching rakudo's own grammar. Adjacency
matters: `Z ?? 2 !! 3` (with a space) is unaffected — `Z` there completes as
its own bare infix and the `??` that follows starts its own term, now
correctly raising `X::Syntax::DuplicatedPrefix`.

With that in place, `?` was safe to add to `duplicated_prefix_run`
(`src/parser/expr/postfix/loop_.rs`): a lone `??` never reaches term position
looking ambiguous anymore, because the meta-op case is intercepted first. `?`
needed one extra guard `^`/`~` don't: `???` is the warn-flavoured yada stub, a
real term (rakudo greedily reads all three as the stub — `????1` is `???`
followed by a bogus postfix `?1`, not a duplicated prefix), so only a run of
*exactly* two `?` counts.

Pin: `t/duplicated-prefix-question-mark.t` (10 assertions, verified to pass
under real `raku` too) — `say ??1` / `my $x = ??1`, `???` staying a real term,
all four `Z??`/`X??`/`R??`/`S??` regression guards, the spaced-`Z` positive
control, and the untouched single prefix `?` / infix ternary `?? !!`.
Verified clean: `roast/S03-operators/ternary.t` (all 28, including the
regression test) and `misc.t`/`precedence.t`/`WHICH.t`, plus
`t/routine-yada.t`, `t/hyper-postfix-dotted-wordy.t`, `t/parser-batch3.t`,
`t/stub-and-supersede.t`.
