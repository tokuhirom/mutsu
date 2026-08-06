# An lvalue sub whose tail is an array element is not assignable

```raku
my @a = 1, 2, 3;
sub elem() is rw { @a[1] }
elem() = 99;
say @a;   # raku: [1 99 3] — mutsu: X::Assignment::RO: sub 'elem' is not rw
```

Pre-existing (verified on the v0.20.0 release binary, 2026-08-06, while
landing the plan-recorded lvalue tail — not a regression from that change).
The assign machinery (`assign_named_sub_lvalue_with_values`) extracts the
routine's tail expression and hands it to `assign_rw_target_expr`, which
handles a plain `Expr::Var` tail but fails for an `Expr::Index` tail
(`@a[1]`), falling through to the "sub is not rw" error. A scalar-variable
tail (`sub f() is rw { $var }`) works.

Fix direction: `assign_rw_target_expr` should assign an Index tail through
the ordinary index-assignment path (the same lvalue evaluation `@a[1] = v`
compiles to), evaluated in the caller's env like the Var case. Note the
tail expr now also arrives via `CompiledRoutineMetadata::rw_tail_expr`
(`news/2026-08/lvalue-tail-from-plan-metadata.md`), so the fix lives purely
in the assign path, not in registration.

Pin to enable: the commented case in `t/lvalue-sub-plan-tail.t`.
