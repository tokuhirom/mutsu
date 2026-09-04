# An autovivifying subscript chain rooted at a method call is dropped

When the root of an lvalue subscript chain is a *method call* rather than a
variable, and the chain has to autovivify the element it descends into, the
write is silently lost. Both the single-subscript and the multi-dim spelling
lose it, so this is not a multi-dim gap — it is the method-call-rooted lvalue
chain itself.

## Repro (measured 2026-09-04, debug build)

```raku
class A { has @.a }

my $o = A.new;
$o.a[0]<x> = 5;      # raku: [{:x(5)},]                 mutsu: []
$o.a[0]{1;2} = 5;    # raku: [{"1" => ${"2" => 5}},]    mutsu: []

# It only survives when the element already exists, because the popped value
# then shares its backing store with the attribute's array:
my $p = A.new;
$p.a[0] = {};
$p.a[0]{1;2} = 5;    # raku and mutsu both: [{"1" => ${"2" => 5}},]
```

`$o.h{1;2} = 5` (no chain, `has %.h`) works for the same aliasing reason: the
attribute hash is already defined.

## Why it is not a one-liner

The single-subscript compiler rewrites a method-call-rooted chain into a call to
the `__mutsu_index_assign_method_lvalue_nested` runtime builtin
(`src/compiler/expr_closure.rs`, the `Expr::Index { target: MethodCall }` arm),
which resolves the accessor and writes back by variable name. That builtin does
not install a freshly autovivified container back into the attribute, so the new
level evaporates. The multi-dim spelling has no such arm at all and falls all
the way through to `OpCode::MultiDimIndexAssignGeneric`.

The correct fix is not to grow the builtin: it is a `runtime/methods.rs`-era
slow path, and CLAUDE.md forbids routing new work through it. What is needed is
for an attribute accessor to yield a real container reference in lvalue context,
so the chain walk that
`OpCode::MultiDimIndexAssignNested` / `IndexAssignDeepNested` already perform for
a variable-rooted chain (`news/2026-09/multidim-assign-through-a-subscript-chain.md`)
can run against it unchanged. That is the "lvalue an arbitrary subscript chain"
machinery several other tickets want, and it needs a design pass rather than a
patch.

## Affected files

- `src/compiler/expr_closure.rs` — `compile_expr_index_assign` (the
  `__mutsu_index_assign_method_lvalue_nested` arm) and
  `compile_expr_multidim_index_assign` (the remaining generic fallback).
- `src/compiler/helpers_ast_utils.rs` — `index_chain_target` /
  `index_assign_target_name` stop at a `MethodCall` root.
- `src/runtime/builtins.rs` (dispatch) and
  `src/runtime/builtins_multidim_assign.rs` --
  `__mutsu_index_assign_method_lvalue_nested`'s implementation.
