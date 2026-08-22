# `but Associative[Int, Int]` (a built-in parametric role) fails as "non-composable"

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Language/structures.rakudoc:258`).

## Root cause

`exec_but_mixin_op` (`src/vm/vm_mixin_does_ops.rs`) decides whether the RHS of `but` is
a role (and should go through `eval_does_values`) using `self.has_role(name)`
(`src/runtime/types/type_registry.rs::has_role`), which only checks the
user-role registry (`registry().roles`). Built-in roles that aren't registered there
(`Positional`, `Associative`, `Callable`, `Iterable`, `Numeric`, `Real`, `Stringy`,
`Mixy`, `Setty`, `Baggy`, `Blob`, `Buf`) are known elsewhere — `is_role_type_name`
(same file, ~line 222) has its own separate `BUILTIN_ROLES` list — but that second list
is only consulted for the *type-object* invocant error path
(`but_on_type_object_error`), not for the main `role_composed` match at the top of
`exec_but_mixin_op`.

So `%hash but Associative[Int, Int]` never enters the `ParametricRole { base_name, .. }
if self.has_role(&base_name.resolve())` arm (since `has_role("Associative")` is
`false`), falls through to the generic "not a role" path, and hits
`mixin_not_composable_error`.

## Minimal repro

```raku
my %not-scalar := %(2 => 3) but Associative[Int, Int];
say %not-scalar.^name;
```

- `raku`: `Hash+{Associative[Int,Int]}`
- `mutsu` (`target/debug/mutsu`): dies with
  `Cannot mix in non-composable type Associative[Int,Int] into object of type Hash`.

## Affected files (starting point)

- `src/vm/vm_mixin_does_ops.rs::exec_but_mixin_op` — the `role_composed` match's role
  checks (`has_role`) should also recognize the built-in roles that
  `is_role_type_name`'s `BUILTIN_ROLES` list already knows about. Consider unifying
  the two role-name checks (`has_role` vs. `is_role_type_name`) so there's a single
  source of truth for "is this name a role (user or built-in)".
- `src/runtime/types/type_registry.rs::has_role` — may need to consult a shared
  built-in-roles list, or `is_role_type_name` needs to be reachable/reused from
  `vm_mixin_does_ops.rs`'s `role_composed` match arms.
