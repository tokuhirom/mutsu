# A smartmatch against a bare `$_` reads the enclosing topic

```raku
for (/a/,) { say ("ab" ~~ $_).raku }
# raku:  Match.new(:orig("ab"), :from(0), :pos(1))
# mutsu: Bool::True
```

And the doc block it was found in
(`raku-doc/doc/Language/structures.rakudoc:220`), which is the same defect one
layer up:

```raku
my @regex-check = (/<alnum>/, /<alpha>/, /<punct>/);
say @regex-check.map({ "33af" ~~ $_ });
# raku:  (｢3｣ alnum => ｢3｣  ｢a｣ alpha => ｢a｣  Nil)
# mutsu: (True True True)
```

## Root cause

Nothing was coercing a `Match` to a `Bool`; the match that would have produced
the `Match` never ran. The RHS of a smartmatch is executed with `$_` already
bound to the LHS — `$x ~~ s///` has to topicalize `$x` so the substitution has
something to write through, and `exec_smart_match_expr_op` installs the topic
before running the RHS instruction range. A bare `$_` on the right therefore
evaluated to the LHS, and `"ab" ~~ $_` really evaluated `"ab" ~~ "ab"`: a
string-vs-string smartmatch, correctly `True`, that never touched the regex the
topic held. `$/` was left unset for the same reason.

Every other spelling was already right — `"ab" ~~ /a/`, a regex read from a
named variable, a regex passed as a parameter, the `WhateverCode` form, and the
same match inside a block. Only the topic itself was affected.

## The rule is on the written shape, not on what the RHS evaluates to

Measured against rakudo v2026.07, and this is the part that decides the fix:

| RHS | raku | why |
| --- | --- | --- |
| `$_` | `Match` | not topicalized |
| `($_)` | `True` | topicalized |
| `$_[0]` | `True` | topicalized |
| `$_<k>` | `False` | topicalized |
| `f($_)` | `True` | topicalized |

So rakudo special-cases the **bare** `$_` spelling — a parenthesised `($_)`
answers `True` there too — rather than exempting every RHS that happens to read
the topic. mutsu already agreed with rakudo on all four of the topicalized
rows; only the bare one was wrong.

`SmartMatchExpr` therefore carries a new compile-time `rhs_is_bare_topic` flag,
set when the RHS expression is exactly `Expr::Var("_")`, and the op skips its
topic install for it. The existing `rhs_is_match_regex` / `rhs_pure_regex`
flags could not be reused: both spellings in the table compile to
`rhs_pure_regex: false`.

Once the topic install is skipped, the bare `$_` evaluates to the enclosing
topic and the ordinary smartmatch runs, so each `ACCEPTS` family answers what
its direct spelling already answered — including the two rows that are *not*
`Match`: a `Set` topic (`"a" ~~ set(<a b>)` is `False`, via `Any.ACCEPTS`) and a
`Str` topic (`Bool`). All eleven families measured now agree with rakudo, where
before only the ones that never reached a regex did.

## Pins

`t/smartmatch-bare-topic-rhs.t` (new, 19 assertions, each also passing under
rakudo v2026.07): the repro and the `.map` form; the parenthesised `($_)`,
`$_[0]` and `f($_)` rows that must keep topicalizing; `$/` being populated; the
`Str`, type-object, `Range`, `Set`, `Block` and `Junction` topics; `!~~`;
nested topics; and `$x ~~ s///` still topicalizing its LHS. The 47 existing
`t/*smartmatch*` and `t/subst-*` files (525 assertions) are green, including
`t/smartmatch-subst-topic.t`, the ADR-0045 pin the topic install exists for.

## Residual

`$_ ~~ $_` over a regex topic still answers a `Match` where raku answers `Nil`,
for an unrelated reason: mutsu's `Regex.Str` returns the regex source text
where rakudo warns and returns `""`, so the stringified LHS `"/a/"` contains an
`a` to find. Filed as
`todo/tickets/regex-str-should-warn-and-return-empty.md`.
