# A parameterized role's BUILD/TWEAK now sees its own type/value parameter when composed via runtime `does`/`but`

```raku
role RP[$v] { has $.p; submethod BUILD { $!p = $v } }
my $q = 1;
$q does RP[42];
say $q.p;   # raku: 42   mutsu (before): (Any)
```

`run_role_submethod` (`src/runtime/types/roles.rs`, invoked by
`call_role_build_submethods` after `$value does Role[Arg]` / `$value but
Role[Arg]` composes a role onto a non-`Instance` value) merged only
`role.captured_env` into the submethod's env before running the body. A
parameterized role's type parameter (`role RP[$v] { ... }`) is not part of
`captured_env` — the ordinary compiled-method dispatch path binds it via
`class_role_param_bindings`, keyed by *class name*, which a plain mixin target
has none of.

Fixed without needing that lookup at all: `compose_role_on_value` (which runs
before the BUILD/TWEAK submethods, earlier in the same file) already stores
each parameter binding on the composed value's own mixin map, under
`__mutsu_role_param__{name}`. `run_role_submethod` now reads those back —
keyed by the role's declared parameter names (`registry().role_type_params`)
— and seeds them into env (restoring any outer binding they shadow)
alongside the existing attribute and `self` seeding, for both single- and
multi-parameter roles.

While verifying, found and filed a separate, unrelated bug in the same
family: a mixin attribute named `sum` reads back through the builtin
`List`/`Cool.sum` method instead of its own accessor
(`todo/tickets/mixin-attribute-named-sum-shadowed-by-builtin-method.md`) —
confirmed not caused by, or a prerequisite for, this fix (renaming the
attribute makes the parameter-binding itself work correctly).

Pin: `t/role-submethod-runtime-does-compiled.t`, extended with a single-param
BUILD case, a single-param TWEAK case, and a two-param case.
