# An lvalue sub whose tail is an array/hash element is assignable

```raku
my @a = 1, 2, 3;
sub elem() is rw { @a[1] }
elem() = 99;
say @a;   # [1 99 3] — was X::Assignment::RO: sub 'elem' is not rw
```

Pre-existing gap (reproduced on the v0.20.0 release binary; not a
regression from the ADR-0019 C6e-3c plan-recorded lvalue tail). The assign
machinery (`assign_named_sub_lvalue_with_values`) extracts the routine's
tail expression and hands it to `assign_rw_target_expr`, which handled a
plain `Expr::Var` tail but fell through to the "sub is not rw" error for
an `Expr::Index` tail (`@a[1]`, `%h<k>`).

Fixed with a new `Expr::Index` arm that synthesizes an
`Expr::IndexAssign` carrying the runtime value as an `Expr::Literal` and
evaluates it in the caller's env — the exact bytecode path `@a[1] = v`
compiles to, so hash-key autovivification and declared element-type
checks (`my Int @a; ... tel() = "nope"` → X::TypeCheck) come for free.
Works for both the trait-carrying def path and the plan-metadata tail
(`CompiledRoutineMetadata::rw_tail_expr`), since both funnel into
`assign_rw_target_expr`.

Pinned by the four new cases in `t/lvalue-sub-plan-tail.t` (array
element, hash element, autoviv, element-type enforcement — verified
against raku).
