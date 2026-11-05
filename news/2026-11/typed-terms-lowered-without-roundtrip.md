# Hyper prefix, atomic variables, object hashes, hyper assignment and inline subsets are lowered where the parser built them

`-« @a`, `⚛$x` / `$x ⚛= v`, `:{ ... }`, `(LVALUES) »=» VALUE` and an inline
`subset :: of ...` term were typed `ParsedExpr` nodes whose lowering is one
function over their operands, but the executing finalization still converted
each to RakuAST and lowered it back. They are now carried and lowered in
`Finalize::parsed_term`: the operands are lowered where they stand, then the
same function the RakuAST lowering uses runs (`lower_hyper_prefix`,
`atomic_op::lower_var`, `lower_hyper_assignment`,
`inline_subset_term::lower`; an object hash is the `__object_hash` call over
its entries). `.AST` and `EVAL($ast)` are unchanged (ADR-15760 slice 2).
