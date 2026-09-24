# Prefix `+` now dispatches `Numeric` on punned roles

`Automata::Cellular` defines `Rule` as a role with a `Numeric` method. A
punned role instance therefore has the right numeric value for an explicit
`.Numeric` call, but prefix `+$rule` incorrectly produced zero in mutsu.

Punned roles are represented by the VM as `Mixin` values. The numeric
coercion opcode only tried the user `Numeric` method for ordinary instances,
so it fell through to the generic coercion path for a role mixin. It now
dispatches role-provided `Numeric` methods before applying that fallback.

The regression is pinned by `t/oo/role/prefix-plus-role-mixin.t`, and
Automata::Cellular 0.2.3 now has parity for all 2/2 test files.
