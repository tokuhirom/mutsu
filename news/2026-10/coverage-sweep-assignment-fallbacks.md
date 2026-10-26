# Coverage sweep: unreachable assignment fallbacks deleted

A second line-coverage run over the whole corpus (`t/` with and without `MUTSU_RAKUAST=1`, the
roast whitelist, `docs/dead-code-coverage.md`) found assignment code that no input reaches because
the typed assignment nodes and `rakuast::assign_lower` own the construct. Deleted:

- `single_target_list_lvalue_stmt` and its `lower_statement` branch: a statement `(*, $a) = @l`
  already lowers through the expression form (`single_target_list_lvalue_expr`).
- `list_lvalue_assign_expr` and the `CallOn` branch that called it.
- `assignment_ro_parts` and the `convert` branch that re-recognized the `__mutsu_assignment_ro`
  desugar block (only the written literal form `120 = 3` is converted).
- The variable arms and the `Z=` branch of `build_meta_assign_expr` (a variable target is built by
  `meta_assignment`; only subscripted targets reach it) and `zip_assign_target_name`.
- The comma continuation after a chained assignment in `parse_assign_expr_or_comma`.

`t/vm/binding/assign-star-lvalue-forms.t` pins the surviving behaviour against rakudo.
