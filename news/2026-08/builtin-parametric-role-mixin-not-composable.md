# The built-in roles compose with `but`/`does` again

`my %not-scalar := %(2 => 3) but Associative[Int, Int]` died with `Cannot mix in non-composable
type Associative[Int,Int] into object of type Hash`. So did the un-parameterised spellings
(`%h but Associative`, `%h but Positional`, `5 but Numeric`, `5 but Callable`), all of which raku
accepts.

## Root cause

Three separate "is this name a role?" answers disagreed, exactly as the ticket suspected:

- `has_role` consulted only the user-role registry, and it was what `exec_but_mixin_op`'s
  composition match used.
- `is_role_type_name` (`src/vm/vm_mixin_does_ops.rs`) kept its own private `BUILTIN_ROLES` list,
  but it was only reachable from the *error* path (`but_on_type_object_error`).
- `extract_role_application` (`src/runtime/types/roles.rs`) — the oracle `does` already used, and
  the only one that understands all the RHS spellings — gated on
  `registry().roles.contains_key(...)`.

So `but` never entered role composition for a built-in role and fell through to
`mixin_not_composable_error`. `compose_role_on_value` had, all along, been tolerating a handful of
body-less builtin role names (`Positional`, `Associative`, …); the machinery was there, nothing
routed to it.

## Fix

One list, one oracle:

- `BUILTIN_ROLE_NAMES` / `is_builtin_role_name` now live in
  `src/runtime/types/type_registry.rs` as the single source of truth;
  `is_role_type_name` consults it instead of its private copy.
- `extract_role_application` accepts a built-in role in every RHS spelling, and gained an arm for
  a parameterised role that arrives as a *bracketed type-object name*
  (`Package("Associative[Int,Int]")`) rather than a `ValueView::ParametricRole` — which is how
  the built-in parametric roles are represented.
- `exec_but_mixin_op`'s hand-rolled three-spelling `has_role` match is gone; `but` now asks
  `is_role_application`, the same oracle `does` asks. That is what makes `but` and `does` agree
  by construction rather than by two parallel matches that drifted.
- `compose_role_on_value`'s tolerated-body-less-role list is derived from `BUILTIN_ROLE_NAMES`
  rather than being a fourth hand-maintained list.

As a bonus of the same work, a parameterised role's type arguments now appear in the composed
name (`role_mixin_suffix_excluding` reads `__mutsu_role_typeargs__`), so `5 but G[Int]` reports
`Int+{G[Int]}` — previously `Int+{G}` — and the ticket's own repro reports
`Hash+{Associative[Int,Int]}` exactly as raku does.

## Known remaining gap

`.^roles` on a role-mixed value still does not list the composed roles (it reports only the base
type's own roles) — for a user role as much as a built-in one. That is a separate introspection
gap, untouched here; `.^name`, `~~`, and method dispatch on the composition are all correct.

Pinned by `t/role-mixin-survival.t`.
