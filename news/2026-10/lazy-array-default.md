# `.default` of a lazy array

`my @l = lazy 1..*; @l.default` died with `X::Method::NotFound`: a lazy array
is held as a lazy list value, which carries no container descriptor, so the
`Array.default` row never saw it. The mutating method entry, which knows the
variable, now answers it from the variable's `is default(...)`, its element
type, or `Any`, without reifying the list (#13806). A follow-up should carry
the default on the lazy list itself, so the row answers it directly.
