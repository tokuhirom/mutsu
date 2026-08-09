# Parameterized role's BUILD submethod can't see its own type/value parameter when composed via runtime `does`/`but`

`run_role_submethod` (`src/runtime/types/roles.rs`, invoked by `call_role_build_submethods` after
`$value does Role[Arg]` / `$value but Role[Arg]` composes a role onto a non-`Instance` value) merges
only `role.captured_env` into the submethod's env before running the body. A parameterized role's type
parameter (`role RP[$v] { ... }`) is not part of `captured_env` — it is bound elsewhere via
`class_role_param_bindings` (looked up by owner-class name in the ordinary compiled-method dispatch,
`src/vm/vm_method_dispatch.rs` around the "Role param bindings" section) — so a `BUILD`/`TWEAK`
submethod reading the role's own parameter through this composition path sees `(Any)`/the parameter's
default instead of the argument actually supplied.

## Repro

```raku
role RP[$v] { has $.p; submethod BUILD { $!p = $v } }
my $q = 1;
$q does RP[42];
say $q.p;   # raku: 42, mutsu: (Any)
```

Verified against Rakudo v2026.06. Confirmed present on `main` at commit `18b6f7745`, identical before
and after ADR-0019 D8-3 (the `run_role_submethod` compiled-body cutover) — not a regression from that
change; a pre-existing gap in how this specific composition path threads role parameters into the
submethod's scope.

## Fix sketch

`run_role_submethod` needs to resolve and seed the role's type/value parameter bindings the same way
`call_compiled_method`'s "Role param bindings" step does (`self.class_role_param_bindings(owner_class)`
/ `...(receiver_class_name)`), or thread the parameterized role's resolved bindings through
`RoleDef`/the mixin map at composition time (`compose_role_on_value`, a few dozen lines above this
function) so `run_role_submethod` can seed them into env alongside the captured closure environment.
Needs a case survey first: does `class_role_param_bindings` even have an entry keyed by anything
reachable from a plain (non-class) mixin target, or does that lookup only work for class-based
composition? If not, this may need a role-id-keyed binding table analogous to the mixin map's own
`__mutsu_role_id__{role_name}` marker (see `role_def_for_mixin_role` in the same file) rather than
reusing `class_role_param_bindings` as-is.

## Why not fixed inline with D8-3

Out of scope for the D8-3 slice (swap `run_role_submethod`'s body execution from tree-walk to the
precompiled bytecode chunk, no behavior change) — this is a distinct missing-context bug that predates
and is orthogonal to that swap, confirmed identical on `main` before it.
