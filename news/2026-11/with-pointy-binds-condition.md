# A `with` / `without` pointy parameter binds its condition

A pointy parameter of a `with` or `without` block over anything but a plain
variable (`without Nil -> $s`, `with foo() -> $x`) was declared by assigning
the condition to it:

- `without Nil -> $s { say $s.raku }` printed `Any`, because assigning `Nil`
  resets a scalar. rakudo prints `Nil`.
- The parameter was writable. rakudo's is readonly.

The parameter now binds the once-evaluated condition through the block's
`given`, as the `else -> $y` branch already did. A `$` parameter holds a list
as an item, as a signature's scalar parameter does (#18390).
