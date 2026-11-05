# A key bound to a bare value refuses assignment on a role-mixed Hash

On a Hash a role was mixed into (`%h does R`), a key bound to a bare value kept
accepting assignments, where rakudo dies with "Cannot assign to an immutable
value" ([#16244](https://github.com/tokuhirom/mutsu/issues/16244)). Three
spots dropped the read-only cell the bind makes:

- `%h<k> := 42` stored the value through `assign_role_mixin_element` as a plain
  element, and the later `%h<k> = 77` was not checked against it. The mixin arm
  now keeps a read-only cell as it arrives and refuses a plain store over one.
- A core `BIND-KEY` reached through a mixin (`$bind(%h, 'k', 42)`, the saved
  method object a role calls on `self`) was routed to the `ASSIGN-KEY` row, so
  nothing was bound. It now runs the `Hash` row's own bind.
- The read-only probes (`hash_element_is_readonly_bound`,
  `array_element_is_readonly_bound`, `element_cell_with_metadata`) now look at the
  aggregate a mixin wraps.

Hash::Util 0.0.5's `t/lock_value.t` goes from 14 to 17 of 17 as `raku` does (its
`lock_value` binds a key and expects the next `%h<a> = 77` to die). The
`t/lock_keys.t` half of the same issue has a different cause (#16255).

Regression test: `t/oo/hash-mixin-bound-key-assign-refused.t`.
