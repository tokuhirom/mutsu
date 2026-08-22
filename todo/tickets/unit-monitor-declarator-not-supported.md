# `unit monitor Foo;` (file-scope form of a user-registered `EXPORTHOW::DECLARE` keyword) is not parsed

## Symptom

`OO::Monitors`' `monitor` declarator (already bundled at `modules/OO-Monitors/`,
see [docs/batteries/oo-monitors.md](../../docs/batteries/oo-monitors.md)) works
fine in block form:

```raku
use OO::Monitors;
monitor Foo { method bar { 42 } }
say Foo.new.bar;   # 42, matches raku
```

but fails in the `unit`-statement (file-scope, semicolon-terminated) form that
real Raku also accepts for any class-like declarator:

```raku
use OO::Monitors;
unit monitor Foo;
method bar { 42 }
```

Under raku this declares `Foo` with the rest of the file as its body. Under
mutsu (`target/debug/mutsu`):

```
Unknown function: monitor
  in block <unit> at -e line 1
```

## Root cause

Traced into the parser (all locations current as of this ticket, 2026-08-22):

- `declare_decl` (`src/parser/stmt/class/class_decl.rs:220-236`), which
  handles any keyword registered via a module's `EXPORTHOW::DECLARE` block
  (`monitor` being the one bundled example today), only implements the block
  form: it does `keyword(&kw, input)` then unconditionally expects
  `class_decl_body`. It never looks for a leading `unit` token at all.
- The **builtin** `unit class`/`unit role`/`unit grammar` forms are handled by
  a completely separate parser, `unit_module_stmt`
  (`src/parser/stmt/class/package_decl.rs:171`), which hardcodes the keywords
  `class`/`role`/`grammar` (`package_decl.rs:176,314,428`). It has no
  knowledge of `declare_keyword_names()` (the registry of user-defined
  `EXPORTHOW::DECLARE` keywords) at all.
- The actual "absorb the rest of the file as the body" behavior lives in
  `stmt_list` (`src/parser/stmt/stmtlist.rs:278`): it only invokes
  `class::unit_module_stmt` when `allow_mainline_capture &&
  starts_unit_class_role_grammar(r)` is true. `starts_unit_class_role_grammar`
  (`src/parser/stmt/idents.rs:19-31`) hardcodes the same three keywords
  (`class`/`role`/`grammar`) and does not consult `declare_keyword_names()`
  either.
- Net effect: for `unit monitor Foo;`, `unit_module_stmt` never matches
  (`monitor` isn't `class`/`role`/`grammar`), and `declare_decl` never matches
  either (its first token check is `keyword("monitor", ...)`, but the actual
  input starts with `unit`). No statement parser claims the line, so it falls
  through to the expression-statement parser, which treats `monitor` as a bare
  function call — hence "Unknown function: monitor".

`unit class`/`unit module` were confirmed still working correctly
(`unit role`'s unit form has a separate, unrelated empty-output issue, out of
scope here).

## Why this matters

Found while evaluating `Log::Async` as the mutsu logging battery candidate
(see [docs/batteries/logging.md](../../docs/batteries/logging.md)):
`Log::Async`'s only runtime dependency, `Terminal::ANSI`, ships
`Terminal::ANSI::Virtual.rakumod` written as `unit monitor
Terminal::ANSI::Virtual;` — the file-scope form. That single line is the
entire reason `use Log::Async` fails to load under mutsu today; every other
part of `Log::Async` and `Terminal::ANSI` loads cleanly.

This is a general parser gap (any `EXPORTHOW::DECLARE`-registered keyword used
in file-scope `unit` form hits it), not specific to `Terminal::ANSI` or
`monitor` — but `monitor` is the only such keyword currently bundled, so it is
the only one that can surface the bug today.

## Fix shape (not yet implemented)

Confirmed to be a small, self-contained, local change — no new design needed:

1. `starts_unit_class_role_grammar` (`idents.rs:19-31`) needs to also match
   any keyword in `declare_keyword_names()`, not just the three builtins.
2. `stmt_list`/`declare_decl` need a path that, on seeing `unit <kw> Name;`
   for a registered keyword, absorbs the remaining statements as the body the
   same way `unit_module_stmt` does for `unit class` (tail-statement capture
   already exists in `stmtlist.rs`; it just needs to be reachable from this
   keyword family too).
3. The `is_unit`/meta-trait bookkeeping that `class_decl_body` applies for the
   block form needs to also apply to whatever `declare_decl` produces for the
   unit form, so the resulting declaration behaves identically either way.

## Repro

```sh
cargo build
timeout 10 ./target/debug/mutsu -e 'use OO::Monitors; unit monitor Foo; method bar {42}'
# Unknown function: monitor
timeout 10 ./target/debug/mutsu -e 'use OO::Monitors; monitor Foo { method bar {42} }; say Foo.new.bar'
# 42 -- block form already works
```

## Discovered via

The logging-battery survey (`docs/batteries/logging.md`, 2026-08-22): testing
whether `Log::Async` loads cleanly under mutsu surfaced this via its
`Terminal::ANSI` dependency.
