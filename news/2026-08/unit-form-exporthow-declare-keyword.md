# `unit monitor Foo;` — the file-scope form of an EXPORTHOW::DECLARE keyword now parses

`OO::Monitors`' `monitor` declarator (bundled at `modules/OO-Monitors/`) worked
in block form but not in the `unit`-statement, semicolon-terminated form that
real Raku accepts for any class-like declarator:

```raku
use OO::Monitors;
unit monitor Foo;
method bar { 42 }
```

mutsu answered `Unknown function: monitor`.

## Root cause

Two parser paths, neither of which knew about the other:

- `declare_decl` (`src/parser/stmt/class/class_decl.rs`), which handles any
  keyword a module registered via `EXPORTHOW::DECLARE`, only implemented the
  block form — it matched the keyword and then unconditionally expected a
  `{ ... }` body. It never looked for a leading `unit`.
- The builtin `unit class` / `unit role` / `unit grammar` forms are handled by a
  separate parser, `unit_module_stmt` (`class/package_decl.rs`), reached from
  `stmt_list` only when `starts_unit_class_role_grammar` says so — and that
  predicate (`stmt/idents.rs`) hardcoded those three keywords, with no knowledge
  of `declare_keyword_names()`.

So for `unit monitor Foo;` neither claimed the line, it fell through to the
expression-statement parser, and `monitor` was read as a call to an undefined
function.

## What changed

`starts_unit_class_role_grammar` now also matches any keyword in
`declare_keyword_names()`, and `unit_module_stmt`'s `unit class` arm accepts a
registered declarator keyword in the same position — a registered keyword is a
package declarator peer to `class`, so it parses identically and only differs by
the `__mutsu_declare_how` marker trait appended to the resulting `ClassDecl`,
which is exactly what the block form already produces. The rest-of-unit body
capture in `stmt_list` then applies unchanged, because it keys off
`Stmt::ClassDecl`.

## Why it mattered

This was the single reason `use Log::Async` failed to load: its only runtime
dependency, `Terminal::ANSI`, ships
`Terminal::ANSI::Virtual.rakumod` as `unit monitor Terminal::ANSI::Virtual;`.
With the fix, `Log::Async` went from 2/17 to 11/17 upstream test files and
`Terminal::ANSI` from 2/8 to 5/8 — enough to bundle both as mutsu's general
logging battery (`docs/batteries/logging.md`,
`news/2026-08/log-async-battery-bundled.md`).

The gap was general, not `monitor`-specific: any `EXPORTHOW::DECLARE`-registered
keyword in `unit` form hit it. Pinned by `t/exporthow-declare-unit-form.t`
(a `unit monitor` module in `t/lib/`, verified to behave identically under
`raku`), with the block form asserted alongside so the two cannot drift.
