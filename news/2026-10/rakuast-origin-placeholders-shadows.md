# RakuAST: statement origins, placeholders, shadowing routines

A survey of the `t/` files that run under `MUTSU_RAKUAST=1` but behave differently from the
ordinary frontend (257 of them, no refusal in sight) found four causes that each change what a
program does, not just how it renders. All four are fixed; the round-trip ratchet gains 51 files.

- **Line numbers.** The round trip dropped every `Stmt::SetLine` marker, so each error,
  backtrace and `callframe` line came out wrong. A statement node now carries its line as a
  hidden `origin` field (`src/rakuast/origin.rs`) that `lower` turns back into the marker; the
  node's rendering is unchanged. The call-site markers the parser stamps on calls are restored
  from it by the same functions the parser uses (`parser::callsite_line_arg`,
  `parser::stamp_call_site_markers`). That also cured `EVAL 'return 5'` inside a sub: its
  argument-less call had lost the marker and took a call path the parser's trees never reach.
- **Placeholders.** `@^a`, `%^h`, `&^cb` and `$:foo` render as rakudo's
  `VarDeclaration::Placeholder::Positional` / `::Named` (the sigil and bare name, every
  positional kind `Positional`), in blocks and in subs. A sub with no signature of its own takes
  its placeholders again — `sub a { $:foo }` rejected `:foo` under the mode — through one
  function (`ast::implicit_placeholder_signature`) shared with the parser. A statement-initial
  `$:foo` no longer declares a stray anonymous `state` variable.
- **Routines named like builtin statements.** `sub take`, `sub say`, `sub die` … called as
  `take(5)` ran the builtin statement once lowered. The unit's own declarations
  (`src/rakuast/declared_routines.rs`) now win, as the parser's scope made them win.
- **`done` in a `supply` block** was lowered to the bare word, which the supply expansion did not
  rewrite onto the emitter ("done without supply or react").

One bug outside RakuAST surfaced on the way: `EVAL 'constant TA = 5'` answered `Nil` (rakudo
`5`). Reordering hoists a declaration ahead of the `SetLine` marker in front of it, which then
trailed the unit and took the tail's value with it; the unit compiler now finds the tail
statement past trailing markers.

`--dump-ast` under `MUTSU_RAKUAST` prints the tree the unit would run as, so the two frontends
can be diffed.
