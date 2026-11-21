# `.origin` of proto routines

A `proto` routine now carries the ranges rakudo 2026.11 gives it. This covers
`proto sub g($) {*}`, `proto sub h(Int $x --> Int) {*}`, `proto method m() {*}`
and `proto sub z(|) { {*} }`:
- the name, the signature and its parameters, and the return type are placed
  as for a `sub`;
- a `{*}` body is an `OnlyStar` over the braces, or inside a written block.

`Stmt::ProtoDecl` now records these positions (`spans`), taken by the same
`routine_spans` reader that `sub` declarations use. Before this, a proto left
everything below its own `Sub` / `Method` without a range (#15780).
