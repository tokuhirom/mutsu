# `True`/`False` can finally be shadowed by a module's `sub EXPORT`

`True`, `False`, `Nil`, `Empty` and `Any` are ordinary CORE-scope **lexical
bindings** in Raku, not syntax keywords. A `use` that exports same-named symbols
legitimately shadows them for the rest of the importing file — the ecosystem
`Logic::Ternary` distribution replaces all of `True`/`Unknown`/`False` with
three-valued-logic objects exactly that way.

mutsu matched them as hardcoded literal keywords in
`src/parser/primary/ident/term_literals.rs` and folded each one straight to an
`Expr::Literal` at parse time. Nothing a module installed later could reach a
value that was already baked into the AST, so `say True` printed the `Bool` no
matter what the importer had `use`d. `Unknown` — not a parser keyword — shadowed
correctly, which is what made the failure so lopsided: `my $T = True;` bound a
secret `Bool`, and `$T ~~ $U` then died with
`Cannot resolve caller ACCEPTS(Logic::Ternary:D: Bool:D)` partway through three
of the distribution's five test files
([#9047](https://github.com/tokuhirom/mutsu/issues/9047)).

## Why the existing approximation could not help

mutsu already approximates the export set of a `sub EXPORT` module statically
(ADR-0087: `collect_unit_scope_routines`, `collect_export_hook_value_terms`).
That machinery is useless here, because `Logic::Ternary` does not *declare* the
names it exports — it computes them:

```raku
Logic::Ternary::{@options[0]} = ...;   # @options[0] is a run-time string
...
@options[0] => ...                      # and so is the returned Map's key
```

There is no declaration site to find. Real Rakudo copes because it runs
`sub EXPORT` at genuine **compile time** of the importing unit, so the computed
value is a real symbol-table entry before the rest of the unit is parsed. mutsu
deliberately runs `EXPORT` at module-load time instead — the simplification
`bind_compile_time_lang`'s doc comment already names — which is *after* parsing.

## What changed

The fix does not try to make the export set knowable. It makes the parse stop
needing to know it, and scopes that cost to the only compunits where it can
possibly matter:

- A module scan now records whether the module computes its exports through a
  `sub EXPORT` hook (`ModuleScanResult::declares_export_hook`, replayed on a
  cache hit like `type_index_incomplete`). A `use` of such a module marks the
  importing compunit's outermost parser scope.
- In a compunit so marked — and **only** there — the five shadowable term
  keywords parse to a new `Expr::ShadowableTermKeyword { name, value }` carrying
  the constant they would otherwise have folded to, which compiles to
  `OpCode::GetShadowableTerm`.
- At run time that opcode prefers a same-named binding the hook installed and
  otherwise pushes the folded constant. The probe is gated on
  `Interpreter::export_term_override_names`, a set populated by
  `install_export_symbol` for sigilless keys, so a same-named env key that got
  there some other way (an `our $True`, whose scalar read is env-keyed without
  its sigil) can never be mistaken for a shadowing import.

Everywhere else in the language — every file that imports nothing through such a
hook, which is essentially all of them — `True` is still a compile-time constant
folded by the parser, and no opcode, env probe or hash lookup was added to its
path. `Inf`/`NaN` stay excluded for the same reason `try_kw` already refuses to
let a user-declared type shadow them: they are numeric literals in Raku, not
bindings.

`Bool::True` is a package lookup rather than the bare term keyword, so the real
enum values remain reachable under their qualified spelling in a shadowed file,
matching Rakudo.

## Pins

`t/modules/import-export/export-hook-shadows-term-keyword.t` (the hook installs
`True`/`False`; the three keywords it does *not* install keep their CORE values
in the same file) and
`t/modules/import-export/export-hook-term-keyword-not-shadowed.t` (the same
fixture told through its `use` arguments to install under different names, so
`True` must be untouched). Both were verified to produce identical output under
`mutsu` and `raku`, as was the issue's own `Map.new('True' => Foo.new)` repro.
