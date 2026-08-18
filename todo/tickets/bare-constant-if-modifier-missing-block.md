# A bare (non-`my`/`our`) `constant` declaration with a statement modifier fails to parse

Found while writing a regression test for
`todo/tickets/constant-statement-modifier-value-lost.md` (now fixed and moved
to `news/2026-08/`). That fix only covers `my constant`/`our constant`; a bare
`constant` declaration followed by an `if`/`unless` statement modifier fails
to parse at all, unrelated to and not touched by that fix (confirmed
pre-existing via `git stash` before the fix was applied — reproduces
identically).

## Repro

```raku
constant $w = 12 if False;
say $w;
```

```
raku:  12
mutsu: ===SORRY!=== Error while compiling -e
       Missing block
       at -e:1
       ------>constant $w = 12 if False; say $w;
                                        ^
```

`unless` reproduces the same way. `my constant $w = 12 if False;` (with the
`my`) parses and now evaluates correctly (see the fixed ticket above) — the
`Missing block` error is specific to the bare (no `my`/`our`) form.

## Root cause (not yet investigated)

The error text ("Missing block") comes from the shared `X::Syntax::Missing`
helper (`src/parser/parse_result.rs::MISSING_BLOCK`), used by several
constructs (`for`, `given`/`when`, ...) that expect a trailing `{ ... }`.
Bare `constant $w = 12` (without `my`/`our`) evidently takes a different
parse path than `my constant`/`our constant` that is not statement-modifier
aware and, on hitting `if`, falls through to some block-expecting parser
instead of `parse_statement_modifier`. Needs `--dump-ast` comparison against
a working `my constant ... if ...` to see where the two paths diverge.

## Severity

Low: bare (unqualified) `constant` declarations are less common than `my
constant`/`our constant`/plain `constant NAME = ...` without a trailing
modifier, and no roast test currently depends on this combination.
