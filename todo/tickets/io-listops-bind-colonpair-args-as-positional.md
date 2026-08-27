# say/print/put/note bind a colonpair argument as positional, so they print it

For the io listops `say`, `print`, `put` and `note`, a colonpair written in the
argument list is bound as a **positional** argument and therefore printed. In
Raku a colonpair is a **named** argument: `say`'s signature slurps it into `%_`
and it never reaches the output.

Every other call shape mutsu already gets right — a user sub binds the same
colonpair as named, and a parenthesised `(a => 1)` is positional in both
implementations — so this is specific to the io listops, not to colonpair
binding in general.

## Measured divergence

| source | mutsu | raku |
| --- | --- | --- |
| `say :a;` | `a => True` | *(empty line)* |
| `say :d, "x";` | `d => Truex` | `x` |
| `say :!d:r, "x";` | `d => Falser => Truex` | `x` |
| `say "x", :a;` | `xa => True` | `x` |
| `say :a, :b;` | `a => Trueb => True` | *(empty line)* |
| `put :a, "x";` | `a\tTruex` | `x` |
| `note :a, "x";` | `a => Truex` | `x` |

Not divergent — these already agree, and bound the scope of the bug:

| source | mutsu | raku |
| --- | --- | --- |
| `say (a => 1), "x";` | `a => 1x` | `a => 1x` |
| `sub f(*@p, *%n) { say @p.elems ~ "/" ~ %n.elems }; f :a, "x";` | `1/1` | `1/1` |

## Root cause

`src/parser/stmt/simple/io_stmts.rs` parses these four listops into
`Stmt::Say(Vec<Expr>)` / `Stmt::Print` / `Stmt::Put` / `Stmt::Note`. That
argument vector is flat: it carries no named/positional distinction, unlike the
general call path, whose `CallArg` enum has a `Named` variant
(`src/parser/stmt/args.rs`). A colonpair therefore arrives at the VM's print
opcodes as an ordinary `Binary { FatArrow }` element and is stringified into the
output like any other Pair.

Note that `(a => 1)` must stay positional, so the fix cannot be "drop every Pair
from the output" — it has to preserve *how the pair was written*, which is the
pair-namedness distinction ADR-0021 introduced. The likely shape of the fix is
to route these statements through the same named/positional argument
representation the general call path uses (or to give the `Stmt::Say` family a
named-argument slot), then have the print opcodes ignore the named ones.

## Why this is filed separately

Found while fixing the parse-level defect in
`news/2026-08/adjacent-colonpair-truncated-the-listop-argument-list.md`. That
fix made `say :!d:r, "x"` *parse* correctly (previously it misparsed as
`(:!d).say(r, "x")` and died with "No such method 'say' for invocant of type
'Pair'"); the remaining wrong output is this separate binding defect, which
predates it and is equally visible in the single-adverb form `say :d, "x"`.
It is argument-binding semantics, not parsing, and touches ADR-0021 territory,
so it was deliberately not folded into that PR.

## Repro

```
$ target/debug/mutsu -e 'say :d, "x"'
d => Truex
$ raku -e 'say :d, "x"'
x
```
