# A bare (non-`my`/`our`) `constant` declaration with a statement modifier now parses

```raku
constant $w = 12 if False;
say $w;
```

```
raku:  12
mutsu (before): ===SORRY!=== Missing block
```

`unless` reproduced the same way. `my constant $w = 12 if False;` (with the
`my`) already parsed and evaluated correctly (see
`news/2026-08/constant-statement-modifier-value-lost.md`) — the error was
specific to the bare (no `my`/`our`) form.

## Root cause

A bare `constant` declaration is dispatched directly from the top-level
`STMT_PARSERS` table (`src/parser/stmt/mod.rs`) — unlike `my constant`/`our
constant`, which go through `my_decl_dispatch.rs`'s own wrapper that calls
`constant_decl` and then explicitly applies `parse_statement_modifier` to
the result. `constant_decl` itself never applies a trailing modifier (by
design — the `my`/`our` wrapper's job). Without that wrapper, a bare
`constant $w = 12 if False;` left `if False;` completely unconsumed after
`constant_decl` returned; the statement-list driver then tried to parse "if
False;" as a brand-new `if` control statement, which requires a trailing
`{ ... }` block — hence `X::Syntax::Missing: Missing block`.

## Fix

Added `constant_stmt` (`src/parser/stmt/decl/constant_subset.rs`), a thin
wrapper that calls `constant_decl` and then `parse_statement_modifier` on
its result, and swapped the `STMT_PARSERS` entry to use it instead of the
raw `constant_decl`. Since `constant_decl` produces a `Stmt::VarDecl`
carrying the `__constant` custom trait, this reuses
`try_split_decl_modifier`'s existing `__constant` special case (added for
the `my constant` fix) — the modifier's condition is dropped entirely and
the declaration evaluates unconditionally, matching real raku's compile-time
`constant` semantics for the bare form too.

Regression test: `t/bare-constant-statement-modifier.t` (5 assertions, all
verified against real raku), covering `if`/`unless`, a scalar and an array
constant, and a no-modifier sanity guard.
