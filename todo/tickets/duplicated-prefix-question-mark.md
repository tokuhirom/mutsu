# `X::Syntax::DuplicatedPrefix` still excludes `??`, and `S03-operators/misc.t` is still open

`news/2026-08/metaop-doubled-infix-base.md` landed the diagnosis for `^^` and
`~~` in term position. Three shapes are deliberately left out, and together they
are exactly what keeps `roast/S03-operators/misc.t` failing under the real
`Test` module (tests 35 and 36 are its only real losses; 38 is a `# TODO`).

## 1. `??` in term position — blocked on `Z??`

rakudo diagnoses a doubled `?` the same way it does `^^`/`~~`:

```
my $x = ??1     X::Syntax::DuplicatedPrefix   prefixes=??
```

mutsu says `X::Syntax::Confused`. Adding `?` to `duplicated_prefix_run`
(`src/parser/expr/postfix/loop_.rs`) makes that case right and immediately makes
another wrong:

```
1 Z?? 2 !! 3    raku: X::Syntax::CannotMeta    with `?` in the set: X::Syntax::DuplicatedPrefix
```

which is `roast/S03-operators/ternary.t` test 28. `??` is not a metaop base (it
must not be — rakudo refuses to meta the ternary), so `parse_meta_op` falls
through to the *bare* `Z` case and the `??` reaches `prefix_expr` looking exactly
like term position. **Fix order: teach `parse_meta_op` to recognise an attempted
meta over `??` and raise `X::Syntax::CannotMeta` there, then add `?` to the
run set.** This is the same shape as the `Z^^` problem that
`news/2026-08/metaop-doubled-infix-base.md` solved, except that the answer is a
typed error rather than a valid base.

Two wrinkles for whoever does it:

* `???` is the warn-flavoured yada stub, a **real term**, so only a run of
  *exactly two* `?` counts. Getting this wrong breaks `t/routine-yada.t` and
  every `Test::Tap`-using file, because the module body contains `???`. (`????`
  and `???1` are `X::Syntax::Confused` in rakudo, which mutsu already gets by
  falling through.)
* rakudo reports only the **first two** characters in `prefixes`: `^^^1` is
  `prefixes => "^^"`, not `"^^^"`. The landed helper already does this.

## 2. `1%^^1` — `%^^1` is lexed as a placeholder hash variable

`roast/S03-operators/misc.t` test 35 is
`throws-like "1%^^1", X::Syntax::DuplicatedPrefix, prefixes => "^^"`, written
without spaces. mutsu never reaches the `^^`: the `^` twigil in
`src/parser/primary/var/sigil_vars.rs` accepts *any* following text, so `%^^1`
(and `%^1`) parse as a variable. The visible consequence is a wrong answer, not
just a wrong error:

```
say 1%^1     raku: 0     (that is `1 % ^1`, i.e. 1 % (0..^1))
            mutsu: Variable '%^1' is not declared
```

Requiring the twigil's name to start an identifier (`c.is_alphabetic() || c ==
'_'`) was tried on 2026-08-03 and **reverted**: it does stop the variable read,
but `%` still does not become an infix afterwards, so `1%^1` degrades into two
statements (`Useless use of constant integer 1 in sink context`). The real
problem is the infix-vs-term decision immediately after `%`, so the twigil guard
has to land together with that. Do not re-apply the guard on its own.

## 3. `555 ~~!~~ 666` — `!~~` is taken as the negated-smartmatch infix

Test 36 is `throws-like "555 ~~!~~ 666", X::Syntax::DuplicatedPrefix,
prefixes => "~~"`. rakudo parses `~~` as the infix, then the term `!~~ 666` as
prefix `!` over the doubled `~~`. mutsu's infix scanner takes `!~~`
(`ComparisonOp::SmartNotMatch`, `parse_negated_meta_comparison_op` in
`src/parser/expr/precedence/chain_cmp.rs`) at the point where rakudo is already
past the first `~~`, so nothing ever reaches term position as `~~`.

## Why these are grouped

All three are the same class of bug: **a diagnosis cannot be added at the point
where the offending text appears, because something upstream has already claimed
it**. The landed metaop fix is the worked example — the check was written,
reverted when `make roast` caught it, the upstream scanner was corrected, and
only then was the check restored. Follow that order here too, and run a full
local `make roast` before pushing: `make test` alone passed with the broken
version.
