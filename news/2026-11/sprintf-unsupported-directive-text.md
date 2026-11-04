# `X::Str::Sprintf::Directives::Unsupported` reports rakudo's `.directive` and wraps its message

Measured against rakudo 2026.09, whose `.directive` for a bad directive is the
format string without its first character (`%ld a big number` gives `ld a big
number`, `a%qb` gives `%qb`), and whose message wraps at a space past 71
columns. mutsu reported only the flags/width/conversion span. A local test had
pinned that guess (`a%qb` -> `q`); it now pins rakudo's value. Found by the
raku-doc differential (`Type/independent-routines.rakudoc`, the `%ld` and
`%*2$d` "NYI" lines).
