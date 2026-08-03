# `Z^^` is one infix token, which is what makes `^^5` diagnosable

Two changes that only work together.

## A metaop's base is matched by longest spelling

`parse_meta_op` picks a metaop's base operator from a list of spellings, longest
first — which is why `...` precedes `..` and `^..^` precedes `^..` there. Three
bases were missing from that list: `^^`, `^` and `===`. A missing base does not
fail loudly; it falls through to the *bare* `Z`/`X` case, and the rest of the
text is then read as a term:

| source | rakudo | mutsu before |
| --- | --- | --- |
| `(True, False) Z^^ (False, False)` | `(True False)` | `Range objects are not valid endpoints for Ranges` |
| `(1, 2) Z^ (3, 4)` | `(one(1, 3) one(2, 4))` | `((1 0) (2 1))` — silently wrong |
| `1 Z^5` | `(one(1, 5))` | `((1 0))` — silently wrong |
| `1 Z=== 2` | `(False)` | a parse error |

`Z^` is the interesting one: mutsu read it as a bare `Z` over the prefix-`^`
term `^(3, 4)`, so it answered a *different program* rather than failing.
`===` had to go in before `==`, and `^^` before the new bare `^`, for the same
longest-match reason.

## Only then can a doubled prefix be diagnosed

`^` and `~` are each both a valid prefix operator and the first half of an infix
(`^^`, `~~`), so rakudo refuses a doubled one in *term* position by name:
`X::Syntax::DuplicatedPrefix`, carrying the run in `prefixes`. mutsu answered
`X::Syntax::Confused` for `~~1` and — worse — silently accepted `^^5` as
`^(^5)`.

This diagnosis was written first, before the metaop fix, and **had to be
reverted**: at the top of `prefix_expr` it passed `make test` but broke valid
metaop code, because while the scanner took only the `Z`, the `^^` arrived in
what looked like term position:

```
1 Z^^ 2       raku: ok                      with the check alone: X::Syntax::DuplicatedPrefix
1 X^^ 2       raku: ok                      with the check alone: X::Syntax::DuplicatedPrefix
1 Z?? 2 !! 3  raku: X::Syntax::CannotMeta   with the check alone: X::Syntax::DuplicatedPrefix
```

The third line was `roast/S03-operators/ternary.t` test 28 going red in
`make roast`. With `Z^^`/`X^^`/`R^^` now claimed as single infix tokens, the
first two are fixed at the source and the check is safe.

`?` is deliberately **not** in the set, even though rakudo diagnoses `??` the
same way. `Z??` has to be `X::Syntax::CannotMeta`, and mutsu's scanner still
falls back to a bare `Z` there rather than recognising the attempted meta — so
claiming `??` would trade one roast file for another. Two further wrinkles are
worth recording for whoever adds it: `???` is the warn-flavoured yada stub, a
real term, so only a run of exactly two `?` would count (getting this wrong
breaks `t/routine-yada.t` and every `Test::Tap`-using file, whose module body
contains `???`); and rakudo reports only the *first two* characters in
`prefixes`, so `^^^1` is `prefixes => "^^"`, not `"^^^"`.

Pin: `t/metaop-doubled-infix-base.t` — the five metaop bases, three
longest-match cases that must not regress (`Z+^`, `Z==`, `Z~~`), the three
term-position diagnoses with their `prefixes`, and three assertions that a
*single* `^`/`~` prefix and the infix `^^` are untouched. It passes under `raku`
as well.

## Still open: `roast/S03-operators/misc.t`

Its tests 35 and 36 remain its only real failures under the real `Test` module
(38 is a `# TODO`), and neither reaches the new check:

* `1%^^1` (no spaces) — mutsu lexes `%^^1` as a *placeholder hash variable*.
  The `^` twigil accepts any following text, so `%^1` parses as a variable too
  and `1%^1` answers `1` where rakudo answers `0` (`1 % ^1`). Requiring the
  twigil's name to start an identifier was tried and reverted: it stops the
  variable read, but the `%` still does not become an infix, so `1%^1` degrades
  to two statements. The infix-vs-term decision after `%` is the real problem.
* `555 ~~!~~ 666` — needs `!~~` to reach term position as `~~`; mutsu's infix
  scanner takes `!~~` (the negated smartmatch) instead.
