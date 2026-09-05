# RakuAST method return types

`method m(--> Int)`, `method m() returns Int`, and `method m() of Int` now
render their return type in `.AST`, closing the read-direction gap
`todo/deep/rakuast-remaining.md` recorded for methods. Before this change any
method carrying a return type was a `.AST` coverage boundary
(`RakuAST: `.AST` does not yet support this construct: method with traits /
private / multi / submethod`), even though the identical `sub` forms had been
closed since 2026-08-22.

## Root cause

The blocker was not in the converter but in the parser. `sub`'s trait parser
records *which spelling* a return type used as an internal pseudo-trait —
`__return_via_trait` for `returns X`, `__return_via_of` for `of X` — because
raku models the three spellings with three different nodes
(`Signature.returns`, `Trait::Returns`, `Trait::Of`) and the converter must
never guess between them.

`src/parser/stmt/sub_param/method_decl.rs` filtered *every* `__`-prefixed entry
out of `MethodDecl.custom_traits`, so by the time a method declaration reached
`src/rakuast/convert.rs` the `-->` and `returns` spellings were
indistinguishable. Since `RakuAST::Method` and `RakuAST::Sub` are both
`RakuAST::Routine`s and carry the same `signature` / `traits` shape, once the
marker survives, the existing `return_type_spelling` / `routine_node` machinery
renders methods with no new node logic at all.

## Change

- `method_decl.rs` keeps `__`-prefixed parser markers in
  `MethodDecl.custom_traits` (user-facing traits like `default` and
  `DEPRECATED` are still extracted into their own fields as before).
- `src/runtime/registration_class_body_method.rs` skips `__`-prefixed entries
  when applying user `trait_mod:<is>` candidates, so an internal marker never
  reaches user code. The gate that decides whether trait application runs at
  all now also ignores markers, keeping a plain
  `method m(--> Int)` on exactly the path it was on before.
- `src/rakuast/convert.rs`'s `MethodDecl` arm reads the spelling with the same
  `return_type_spelling` helper the `SubDecl` arm uses and passes the return
  type to `routine_node`. A method with a `__return_via_*` marker but no return
  type is refused rather than rendered wrong.

## Coverage

`t/rakuast-method-return-type.t` (16 assertions) pins all three spellings, the
parameterised forms, the in-class-body form, that a plain `method m() { 1 }`
still omits its signature entirely, and that `Signature.returns` /
`Trait::Returns.type` are reachable through the accessors. Like the rest of the
`t/rakuast*.t` suite it is a dual-oracle test: it passes verbatim under both
mutsu and the system `raku`.

The write direction is untouched: `RakuAST::Method` has no `EVAL` lowering at
all yet (neither does `RakuAST::Class`), so method lowering remains its own
future slice rather than a gap this change opens.
