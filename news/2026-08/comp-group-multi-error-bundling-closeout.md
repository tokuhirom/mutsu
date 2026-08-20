# `X::Comp::Group` multi-error bundling is supported — closing a stale deep finding

`todo/deep/comp-group-multi-error-bundling-unsupported.md` claimed that mutsu's
parser "stops at the first fatal error and reports one diagnosis — there is no
mechanism to accumulate more than one", and that closing the two
`roast/S32-exceptions/misc.t` gaps it named would require a parser-wide
multi-error accumulation capability plus an ADR. Re-verified against `main` at
`f6f66b265` (2026-08-20): **the premise is stale.** The mechanism exists, is
general, and is used in a dozen places; both named cases behave correctly; and
the one genuinely-open residual has a different root cause that is already
tracked by its own deep ticket.

## The mechanism that now exists

`Value::make_comp_group(message, panic, sorrows, worries)`
(`src/value/value_methods_b.rs`) builds an `X::Comp::Group` instance carrying
`.sorrows` / `.worries` / `.panic`, and `PError::comp_group` /
`PError::comp_group_with_panic` (`src/parser/parse_result.rs`) wrap it as a
fatal parse error. Its doc comment records rakudo's actual collapsing rule —
accumulate worries, sorrows and at most one panic; collapse to a single
exception only when exactly one thing was collected, otherwise emit a group —
and warns against using it where rakudo genuinely raises a lone panic. Roughly
a dozen parser and runtime sites build groups through it today: illegal decimal
points, unterminated regexes, bare `say`, VCS conflict markers, duplicate `of`
type declarations, block-gobbling barewords, `for`-loop missing blocks,
statement-control keywords in term position, sub-name worries, regex adverb
cascades, and the runtime regex parser's sorrow accumulator. `X::Comp::Group`
is registered as an exception class in `runtime_init.rs` and known to
`type_constraints.rs`.

## Case 1 — `5.` — works

```
$ mutsu -e 'my $e = try { EVAL q{5.}; Nil } // $!; say $e.^name; say $e.sorrows[0].^name'
X::Comp::Group
X::Syntax::Number::IllegalDecimal
```

That is exactly the shape the roast assertion
(`throws-like '5.', X::Comp::Group, sorrows => sub (@s) { @s[0] ~~ X::Syntax::Number::IllegalDecimal }`)
checks, and `t/decimal-point-illegal-comp-group.t` pins it locally. One cosmetic
fidelity gap remains — mutsu's `.panic` is `X::Comp::AdHoc`/`"Confused"` where
rakudo's is `X::Syntax::Malformed`/`"Malformed postfix call"`, so the group's
`.message` carries only the first of rakudo's two lines. Nothing asserts on
`.panic`, so this is a small polish item, recorded as
`todo/tickets/illegal-decimal-comp-group-panic-shape.md`.

## Case 2 — `when SomeUndeclaredType { ... }` — different root cause, already tracked

`roast/S32-exceptions/misc.t`'s
`throws-like 'given 42 { when SomeUndeclaredType { 1 }; default { 0 } }', X::Comp::Group, :message(/SomeUndeclaredType/)`
passes, but running the same source directly still produces no diagnosis at all
(mutsu falls through to `default` and exits 0). The obstacle is *not*
multi-error bundling: `given_when.rs` already builds precisely the right
`X::Comp::Group` (an `X::Syntax::BlockGobbled` sorrow plus an `X::Syntax::Missing`
panic) via `gobbled_block_error`, but deliberately fires it only for barewords
under the reserved `X::`/`CX::` namespaces, because mutsu registers
imported/cross-file types at run time and cannot tell a genuinely undeclared
name from one declared in a sibling file of the same distribution. That is a
**compile-time name-visibility / cross-file type index** problem, and it is
documented in full — including the batteries survey showing why the obvious
broadening regresses `Cro::HTTP::ResponseParser`'s `when Header { ... }` — in
`todo/deep/when-undeclared-bareword-gobbles-block-needs-cross-file-type-index.md`.
Keeping a second deep ticket for the same residual under a misleading
"bundling is unsupported" title would only send the next reader down the wrong
path.

## Status of the roast files that motivated the finding

`roast/S32-exceptions/misc.t` is whitelisted and passes cleanly (182 tests).
More broadly, 16 roast files mention `X::Comp::Group`, and 15 of them are
whitelisted today; the sole exception is `roast/S05-mass/rx.t`, which
`TODO_roast/BLOCKERS.md` records as a non-goal because rakudo itself cannot
compile it (`::` backtracking control is NYI upstream). So the "5 known roast
files blocked by `X::Comp::Group`" figure that `todo/TRIAGE.md` carried for this
ticket is zero addressable files.

## Note on `throws-like`'s `X::Comp::Group` broadening

`src/runtime/test_functions/throws_like.rs` deliberately treats an expected
`X::Comp::Group` as satisfied by any class that does the `X::Comp` role. That
broadening is why case 2's roast assertion passes even though the exception
mutsu actually raises through `EVAL` is `X::Undeclared::Symbols`. It is
pre-existing, intentional, and commented as such — flagged here only so a future
reader does not mistake a passing roast assertion for full rakudo parity on the
underlying diagnosis.
