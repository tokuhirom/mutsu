# `BIND-POS` to a bare value makes the element immutable

`@a.BIND-POS($i, 42)` now leaves an element that refuses assignment, as in
rakudo: `@a[$i] = 3`, `@a.ASSIGN-POS($i, 3)` and a `for @a { $_ = ... }` alias
all die with "Cannot assign to an immutable value", and the bound value stays
(#10924). `BIND-POS` used to wrap the value in a `Scalar` marker that only the
`ASSIGN-POS` method checked, so the `[]=` store silently overwrote it.

The element now holds a read-only container cell (`Value::bound_element`), the
same representation `%h.BIND-KEY($k, 42)` already used for a hash, so the
read-only-ness travels with the element itself; a `BIND-POS` to a container (a
variable's cell, an `is rw` return) still shares that container. The `[]=` path
checks it with `array_element_is_readonly_bound`, the positional twin of the
hash check. `++` on such an element, and the older name-keyed marker `@a[i] :=
42` uses, are tracked in #10984.
