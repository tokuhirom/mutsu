# A sigilless parameter bound to a `for` topic writes the element: pinned

`for @b { g($_) }` with `sub g(\s) { s = "X" }` used to bind a copy, so the
write was lost, and `for @b { .&g }` died "Cannot modify an immutable Str". The
recent sigilless-alias and code-object-call fixes on `main` already made both
write through to the array element, as do the `is rw`, `<->` and
`-> $x is rw` shapes. A regression test now pins all of them (#11447).
