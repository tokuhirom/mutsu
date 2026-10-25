# convert renders a routine's written traits only

`convert` had two ways to render the traits of a `sub`, `method`, `proto` or `module` / `package`:
the record of the traits as the source wrote them (`ast::routine_trait`, kept by every parse that
keeps spellings), and a reconstruction from the execution fields the parser folds them into
(`is_rw`, `is_raw`, `export_tags`, `associativity` / `precedence_trait`, `custom_traits`, the
`__return_via_*` spelling markers, `is default` / `DEPRECATED`). The reconstruction refused whenever
the source order of two traits could not be recovered ("their source order is not kept").

Over the 8,702 `t/` files of the `MUTSU_RAKUAST=1` ratchet the reconstruction was never reached, so
it is gone: `add_custom`, `method_custom_traits`, `IsTraits::with_precedence`, `ReturnSpelling` and
`return_type_spelling`, and the `is_export` / `export_tags` plumbing of `proto::convert`. A routine
or package whose folded fields name a trait but whose body carries no written-trait record is now
refused, which can only be a tree that was not built by a spelling-keeping parse.

`MUTSU_RAKUAST=convert` is new: the executing round trip without the passthrough leaves
(ADR-10723 section 6.109), so every node goes through `convert` and `lower` as `.AST` and
`EVAL($ast)` do. `MUTSU_RAKUAST=1` stays the cost-measured mode; `convert` keeps the coverage of the
`convert` / `lower` arms of the families the passthrough carries.
