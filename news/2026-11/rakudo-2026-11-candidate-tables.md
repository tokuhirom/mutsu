# Routine candidate and method tables regenerated on Rakudo 2026.11

The tables that describe rakudo's core routines and the method tables of its
built-in classes were 2026.09 snapshots, though the pinned oracle
(`.rakudo-version`) is 2026.11. They are now regenerated on 2026.11 (#19274):

- `src/builtins/rakudo_candidates.txt` (the data behind `&say.candidates`,
  `.cando`, `.file`/`.line` of a built-in and core junction autothreading)
  now covers every owner the method-table snapshot lists. Owners added to
  that snapshot after the 2026.09 run (`Promise`, `Proc::Async`, `Lock`, the
  `CompUnit` classes, the RakuAST nodes, ...) had no candidate rows until
  now. The core subs gain 2026.11's new candidates: `infix:<&&>`, `||`,
  `^^`, `//`, `and`, `or` and `xor` with a `WhateverCode` right operand,
  `infix:<**>(Num:D, Int:D)`, `eqv` on `Array::Jagged`, and the native
  shaped-array subscripts.
- `src/builtins/rakudo_method_tables.txt` drops a duplicated `Any` block and
  picks up `Array`/`Hash`'s `ELEMENT-DESCRIPTOR`, `Hash`'s `VIVIFY-*-KEY`,
  `Rakudo::Internals`'s `JAGGED-*` helpers, the reworked RakuAST `IMPL-*`
  methods, and `IO::Path.resolve-inside`.
- `src/builtins/rakudo_method_candidates.txt` and
  `src/runtime/builtin_method_sigs.tsv` follow 2026.11's signatures: `Any`'s
  `BIND-KEY` binds `Mu \BIND`, `Hash` gains `:U` `ASSIGN-KEY`/`BIND-KEY`
  candidates (autovivifying a type object), and `Seq.from-loop` takes
  `:$until` and `:$pass-condition`.

`IO::Path.resolve-inside($io)`, which 2026.11 added, is implemented as a
method row: the invocant resolved completely when it lies strictly inside the
existing directory `$io`, and `Nil` otherwise.
