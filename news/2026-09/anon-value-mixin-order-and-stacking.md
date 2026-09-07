# An anonymous `but <non-role>` mixin has an application order, and two of them stack

`but <non-role>` composes a fresh anonymous role in raku, and each application
is its own composition. mutsu recorded them all under a single
`__mutsu_value_mixin__` key, which cost two things:

```raku
role A { }
say ((1 but "x") but A).^name;
# raku:  Int+{<anon|1>}+{A}      mutsu was: Int+{A}+{<anon|1>}

say ((1 but "x") but "y").^name;
# raku:  Int+{<anon|1>}+{<anon|2>}   mutsu was: Int+{<anon|2>}
```

The key held no application stamp, so `role_mixin_suffix_excluding` and
`mixin_roles_applied_last_first` both appended it after every named role — right
only when the anonymous one happened to be applied last. And being one key, a
second `but "y"` simply overwrote the first's minted name, so the two
compositions were reported as one.

## The fix

The marker did double duty, and only one of its jobs needed it to be a single
key: its mere *presence* is what tells a value mixin from a genuine allomorph
(`<42>`, `val("42")`), which is built with the same `{Str => …}` shape and must
keep reporting `IntStr`. That job is unchanged — `allomorph_type_name`,
`type_matching.rs` and `types_isa.rs` still read exactly the same flag.

The *display* job moved to a per-application family,
`__mutsu_anon_role__<anon|N>`, one key per composed anonymous role. Each entry
carries the same `__mutsu_role_seq__{name}` and `__mutsu_role_group__{name}`
stamps a named role gets (each `but` being its own application, the group id is
the sequence stamp itself), so the anonymous roles simply take their place in
the existing sort: the name interleaves them correctly, `.^roles` lists each one
separately in the same last-first order, and two of them no longer share a key
to overwrite. `filter_composition_markers` carries the new markers onto the
shared `.WHAT` node so it renders its own name the same way.

Identity is unmoved: the flag still holds the freshly minted name, so
`(1 but "x") === (1 but "x")` stays `False`, as in raku.

Pinned by `t/anon-value-mixin-order-and-stacking.t`, whose 14 assertions pass
unchanged under rakudo (they match the *shape* of the name, since the
`<anon|N>` counter is per-process). `t/mixin-name-application-order.t`,
`t/mixin-value-identity.t` and the other 26 `t/mixin-*` files are unmoved.
