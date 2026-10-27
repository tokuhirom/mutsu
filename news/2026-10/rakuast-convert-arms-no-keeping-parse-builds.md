# convert drops the arms for assignment forms no keeping parse builds

A parse that keeps spellings (what `.AST` and the `MUTSU_RAKUAST` round trip read) builds the typed
assignment nodes (#14948), so `convert` never again sees the executing parse's lowered forms of the
same constructs. A coverage run over the whole corpus (`docs/dead-code-coverage.md`: `t/` under both
frontends, the roast whitelist and a parse-only pass over every roast, vendor and batteries file)
confirms it. Gone from `src/rakuast/`:

- the `Expr::SymbolicDerefAssign`, `Expr::IndirectTypeLookupAssign` and `Expr::CompoundAssign` arms of
  `convert_expr` (with `symbolic_deref::convert_assign` / `convert_type_assign`, `is_dotty_assign_op`);
- the `DoStmt` of a declaration-only `SyntheticBlock` arm, which rebuilt a comma list of declarations
  the typed `CommaDecls` node now carries;
- `hyper_dotty_writeback.rs`, a recognizer for the `@a[$i]».=method` writeback expansion (64 lines, none
  of them ever run);
- the anonymous-routine fallback that rebuilt `is rw` / `is raw` from the execution fields when no
  written-trait record existed (`IsTraits::nodes`, `routine_traits::add_flags`'s trait parameter and
  its source-order refusals). The same run with the fallback replaced by a refusal passed all 8,702
  `MUTSU_RAKUAST=1` ratchet files.
