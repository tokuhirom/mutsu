# CSS::Properties `t/declarations` and `t/calc` match rakudo

Three general fixes found by running the distribution next to `raku`:

- A buffer-backed class that declares its own `list` (upstream's `CArray`) now answers
  `Any`'s `self.list` methods (`Array`, `List`, `join`, `kv`, `flat`, ...) from it. They
  used to wrap the array in a one-element list, and `.join` recursed until the stack ran out.
- `$str-with-role[0]` is the value itself, role included (`Any.AT-POS(0)`), instead of the
  bare inner `Str`.
- `get()[1] = v` and `$obj.m[1] = v` store through an element bound to a `Proxy` (its
  `STORE` runs) instead of replacing the binding with a plain value.

(The multi-tier fix is in `multi-tier-bind-check-no-ambiguity.md`.)
