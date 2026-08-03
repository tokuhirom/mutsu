# `X::Syntax::DuplicatedPrefix` needs a metaop-aware place to live

`^`, `~` and `?` are each both a valid *prefix* operator and the first half of
an infix (`^^` / `~~` / `??`), so rakudo refuses a doubled one in term position
by name — `X::Syntax::DuplicatedPrefix`, carrying the run in `prefixes`:

```
my $x = ~~1     X::Syntax::DuplicatedPrefix   prefixes=~~
my $x = ^^^1    X::Syntax::DuplicatedPrefix   prefixes=^^^
say ^^5         X::Syntax::DuplicatedPrefix   prefixes=^^
```

mutsu answers `X::Syntax::Confused` for the first two and, worse, *silently
accepts* `^^5` as `^(^5)` — a different program. `roast/S03-operators/misc.t`
tests 35/36 ask for the class with the `prefixes` attribute
(`throws-like "1%^^1", X::Syntax::DuplicatedPrefix, prefixes => "^^"`).

## What was tried, and why it was reverted (2026-08-03)

Raising the diagnosis at the top of `prefix_expr`
(`src/parser/expr/postfix/loop_.rs`) gets the term-position cases right and
`make test` passes — but it **breaks valid metaop code**:

```
1 Z^^ 2     raku: ok        mutsu with the check: X::Syntax::DuplicatedPrefix
1 X^^ 2     raku: ok        mutsu with the check: X::Syntax::DuplicatedPrefix
1 Z?? 2 !! 3  raku: X::Syntax::CannotMeta   mutsu with the check: DuplicatedPrefix
```

(the last one is `roast/S03-operators/ternary.t` test 28, which regressed in
`make roast`). The cause is that rakudo scans `Z^^` as **one infix token**,
so the `^^` is never in term position at all, whereas mutsu's infix scanner
consumes only `Z` and then calls `prefix_expr` on `^^ 2`.

So the diagnosis cannot live in `prefix_expr` until the metaop scanner claims
the whole `Z^^` / `X^^` / `R~~` sequence as an infix. **Fix that first** — it is
a bug in its own right: `1 Z^^ 2` today parses as `1 Z ^(^2)` and dies with
`X::Range::InvalidArg` instead of zipping two booleans.

The pieces that were written and can be recovered from the revert:

* `RuntimeError::duplicated_prefix(prefixes)` — the message is
  `Expected a term, but found either infix ^^ or redundant prefix ^` plus a
  second line suggesting the spaced spelling, and the exception carries
  `prefixes`.
* The run test itself, whose one wrinkle is `?`: `???` is the warn-flavoured
  yada stub (a real term), so only a run of *exactly two* `?` is a duplicated
  prefix, while `^^^` and `~~~` are duplicated at any length. `????` and `???1`
  are `X::Syntax::Confused` in rakudo, which mutsu already gets by falling
  through. Getting this wrong breaks `t/routine-yada.t` and every
  `Test::Tap`-using file (the module body contains `???`).

## Blocked file

`roast/S03-operators/misc.t` — tests 35 and 36 are its only real failures under
the real `Test` module (test 38 is a `# TODO`), so the class plus the `prefixes`
attribute closes it. Note test 36 (`555 ~~!~~ 666`) needs the `!~~` spelling to
reach term position as `~~`, and test 35 (`1%^^1`, no spaces) needs `%^^1` to
stop being read as the placeholder variable `%^^1` — both separate from the
metaop issue above.
