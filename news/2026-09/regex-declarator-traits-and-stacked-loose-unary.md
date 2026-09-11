# Two parse failures off the `blocked_load` index: regex-declarator traits and a prefixed whatever-curry

[#7954](https://github.com/tokuhirom/mutsu/issues/7954) is the located index of the largest single
`blocked_load` bucket from the first full-corpus ecosystem sweep: 56 distributions whose load dies
on a hard parse error. Three of its rows were reduced to one-liners that diverge from rakudo on
their own; one of those (`BEGIN` writing an `our`-scoped `%`/`@`) was already filed separately as
[#7953](https://github.com/tokuhirom/mutsu/issues/7953). This entry is the other two.

## 1. `my token X is export { ... }` — a regex declarator took no traits at all

`Collection` (`lib/Collection/Entities.rakumod:11`) and `Number::More`
(`lib/Number/More.rakumod:21`) both open with an exported token:

```raku
my token all-bases is export(:token-all-bases) { ... }
```

`token_decl` went straight from the (optional) signature to the `{ ... }` body, so the `is` landed
in expression position and the whole compilation unit died with `Confused. expected statement`.
That is the worst shape a parse gap can take: one unimplemented trait costs the entire
distribution, not the one declaration.

A regex declarator is a routine, so it now takes the routine trait grammar — the same
`parse_sub_traits` that `sub`, `method` and `proto token` already use. `is export` is the trait with
semantics here; every other one is accepted and dropped, which is strictly better than reinstating
the hard failure for the next trait nobody has implemented yet.

Making it *parse* is only half the row. In rakudo, `my token digits is export` exports a `Regex`
under `&digits`, and both `&digits` and `<digits>` resolve in the importer. mutsu keeps regex
declarators in `Registry::token_defs` rather than `Registry::functions` (ADR-0009: a token has no
compiled body), and a *lexical* one is dropped by the block-scope restore when the module's own
scope exits — so an export table entry alone would have imported nothing. The defs are therefore
captured at declaration time (`Interpreter::exported_token_defs`, keyed by the module being loaded)
and re-installed under the importing package by `import_module`. Since `&name` is a lazy by-name
`Routine` that resolves through `resolve_token_defs`, that one registration is what makes both
spellings work:

```raku
use ExportedRegexDeclarator;         # my token digits is export { \d+ }
say ~("abc42" ~~ &digits);           # 42
say so "abc42" ~~ / <digits> /;      # True
```

Tag filtering rides on the existing export machinery, so `is export(:wordy)` stays hidden from a
plain `use` and arrives with `use Mod :wordy`.

Pin: `t/grammar/token-declarator-is-export.t` (8 assertions, green under rakudo too), with the
module fixture `t/lib/ExportedRegexDeclarator.rakumod`.

## 2. `!so *` — `so` and `not` did not stack under a tighter prefix

`Linux::NFTables` (`lib/Linux/NFTables.rakumod:29`) constrains a parameter with a prefixed
whatever-curry:

```raku
multi method buffer-output(Bool $active where !so * --> Bool)
```

`so *` on its own parsed; `!so *` did not. `so` and `not` are prefix operators in Raku, and prefixes
stack — `!so *` is one prefix chain over the term `*`, whose precedence is that of the *loosest*
prefix in it. mutsu only knew `so` at loose-unary statement level, so `!`'s operand was parsed as an
ordinary tight prefix expression, `so` was read as a bare term, and the `*` that followed became an
infix multiply with nothing on its right — "expected expression after multiplicative operator".

`prefix_expr` now recognises a loose `so`/`not` in operand position and parses the rest of the chain
at loose-unary precedence. The curry then composes over the whole chain, matching rakudo:

```raku
my $c = !so *;   say $c.WHAT;   # (WhateverCode)
say $c(False);                  # True
say !so 1 == 2;                 # True — still !(so(1 == 2))
```

The existing reading of a non-Whatever operand is unchanged (and, if anything, tightened towards
rakudo: the old identifier-call path swallowed its argument at list-prefix precedence, looser than
loose unary).

Pin: `t/lang/operators/prefix-stacked-loose-unary.t` (9 assertions, green under rakudo too).

## Scope

#7954 is an index, not one fix, and stays open: the remaining rows span roughly twenty distinct
constructs, and the two closed here are the ones the issue had already confirmed in isolation.
