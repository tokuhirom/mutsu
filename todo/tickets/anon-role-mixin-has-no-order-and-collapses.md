# An anonymous `but <non-role>` mixin has no application order, and a second one overwrites the first

Found 2026-09-07 while fixing
`news/2026-09/mixin-name-renders-application-order.md`, which gave the NAMED
roles a per-application order and grouping. The anonymous role a
`but <non-role>` mints was left out of that: it is recorded under a single
`__mutsu_value_mixin__` key with no sequence stamp, so it can neither be ordered
against the named roles nor coexist with a second one.

## Repro — the order

```raku
role A { }
say ((1 but "x") but A).^name;
# raku:  Int+{<anon|1>}+{A}
# mutsu: Int+{A}+{<anon|1>}
```

The reverse spelling agrees, because the anon is appended last and that happens
to be right there:

```raku
say ((1 but A) but "x").^name;   # both: Int+{A}+{<anon|1>}
```

## Repro — the collapse

```raku
say ((1 but "x") but "y").^name;
# raku:  Int+{<anon|1>}+{<anon|2>}
# mutsu: Int+{<anon|2>}
```

Two `but <non-role>` applications compose two distinct anonymous roles in raku.
mutsu keeps one `__mutsu_value_mixin__` key, so the second application
overwrites the first's minted name and the composition is reported as a single
anonymous role.

## Where to look

`Interpreter::apply_single_mixin` (`src/vm/vm_mixin_does_ops.rs`) writes the
marker; `role_mixin_suffix_excluding` and `mixin_roles_applied_last_first`
(`src/value/types.rs`) both append it last for exactly this reason, and
`mixin_composition_key` / `mixin_identity_key` treat it as one opaque entry.

## Why it is not a one-liner

The marker does double duty: its *presence* is what distinguishes a value mixin
from a genuine allomorph (`<42>`, `val("42")`), which is built with the same
`{Str => ...}` shape and must keep reporting `IntStr` — see the comment on
`allomorph_type_name`. Turning it into a per-application family of keys means
finding a new home for that "this is not an allomorph" signal, and re-checking
every reader of it (`type_matching.rs`, `types_isa.rs`, the two name/identity
key builders).

## Check when fixing

Both repros above; `(1 but "x")` alone is still `Int+{<anon|1>}`; a genuine
allomorph (`<42>`, `val("42")`) still reports `IntStr` and still does `Str`;
`(<42> but R).^name` is still `IntStr+{R}`; `.^roles` lists each anonymous role
separately and in the same order the name does; and
`t/mixin-name-application-order.t` plus `t/mixin-value-identity.t` still pass.
