# X::Comp gains .pre/.post; X::InvalidType and X::Syntax::Adverb gain their missing attributes

Continuing the `todo/tickets/vendor-real-test-module.md` campaign (replacing
mutsu's native `Test` provider with the real, vendored `Test.rakumod`),
`roast/S32-exceptions/misc.t` was crashing under `MUTSU_REAL_TEST=1` on three
independent "typed exception, missing attribute" gaps — the same shape as
several earlier fixes in this campaign, where `throws-like` reads an attribute
that the class was never given, and a single missing method aborts the whole
test file instead of just failing one subtest.

1. rakudo's `X::Comp` base class — the ancestor of the whole `X::Syntax::*`
   family — carries `.pre`/`.post` (the source text immediately around a
   parse failure's eject point), but mutsu's generic derivation of attributes
   from the `"X::Type: text"` message convention only ever derived
   `X::Syntax::Missing.what`. `parser::parse_program()` is the one place that
   unambiguously has both the full original source and the failure offset for
   a recoverable, typed-convention-message parse diagnosis, so it now computes
   `pre`/`post` there and carries them on two new `RuntimeError` cold fields;
   the generic attribute derivation fills them in for any class that doesn't
   already set its own (so existing call sites that build a fuller exception
   themselves are untouched).
2. `X::InvalidType` had `.typename` on its `returns`/`of`-trait raise site but
   not on its `does`/`hides`-parent one. Fixed by deriving it from the message
   text (`"Invalid typename '{typename}'"`), the same "derive it the way
   rakudo derives it so the two cannot disagree" rule already used for
   `X::Syntax::Missing.what`.
3. `X::Syntax::Adverb` had no `.what` at either of its two raise sites (`my $x
   :a`, and `infix:(&)` under `use MONKEY`).

`roast/S32-exceptions/misc.t` now passes fully under both the native and the
real `Test` module. Pin: `t/typed-exception-attributes.t` gained three new
cases (16 → 21 assertions), green under `raku` too except the `pre`/`post`
case, which exercises a construct where rakudo itself gets the eject position
wrong (rakudo issue #4431, `#?rakudo todo`'d in the roast source) — the pin
asserts mutsu's own correct eject point instead of reproducing rakudo's bug.
