# RakuAST: control flow and phasers

Loops, conditionals and phasers now read back as the nodes rakudo has
(measured on rakudo 2026.09), and the lowering rebuilds the parser's own
expansion, so the round trip is the parsed program.

- **Statement modifiers.** `STMT for LIST` is the statement with a
  `loop-modifier => StatementModifier::For(LIST)` instead of a `Statement::For`
  around a block, also inside `[...]` and `(...)`, and a block modified by a
  `for` (`{ ... } for LIST`) keeps its arity. `parser::for_modifier_loop_params`
  is the one builder of the placeholders such a loop takes, so a lowered
  modifier loop is the parser's loop.
- **`for` loops.** `hyper` / `race` / `lazy for`, `<->` (`default-rw`
  parameters) and `for @a -> { }` (a pointy block with no signature) render and
  lower; labelled `for` / `while` / `until` / `loop` / `repeat` keep their
  `labels` and a labelled `last` / `next` / `redo` is the call over the label
  as a `Term::Name`.
- **`CONTROL { ... }`** is a `Statement::Control` over the same exception
  block as `CATCH`.
- **`if EXPR -> $v { }`** (and `elsif`) has a `PointyBlock` for its `then`.
- **Phasers in expression position** (`my $x = BEGIN { 1 }`) and `once { }`
  (`StatementPrefix::Once`).
- **`do for` / `do given` / `do if`**, and loops or conditionals used as
  expressions (`my $s = (given 5 { ... })`), are a `StatementPrefix::Do` over
  the statement; the lowering gives back the `DoStmt` the parser builds.
- The array composer and parentheses answer `.semilist`, as in rakudo.

Left for later in the plan (not S5): `with` / `without` with an explicit
signature, `if EXPR -> \v` / `-> $_` / typed / `is copy` bindings, `PRE` /
`POST`, a labelled `do` block, `orwith` — S10; the `required-topic` flag and
other text differences — S9.

New test: `t/rakuast/rakuast-control-flow.t`, whose tree part also runs under
`raku`. Slice S5 of #7564.
