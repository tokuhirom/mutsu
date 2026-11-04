# One "cannot unbox" message, and `chmod` coerces its mode with `Int()`

Two gaps found by the raku-doc differential (`Language/numerics.rakudoc`,
`Type/IO/Path.rakudoc`):

- Eight sites raised `Cannot unbox N bit wide bigint into native integer`; five
  of them had dropped rakudo's trailing `Did you mix int and Int or literals?`.
  They now share `RuntimeError::cannot_unbox_bigint`.
- `IO::Path.chmod` and `sub chmod` read a `Str` mode as an octal string, so the
  documented trap `chmod '0444'` (which rakudo reads as the decimal 444, mode
  `0o674`) silently worked, and `'0o444'` produced mode `0000`. Both now share
  `chmod_mode`, which coerces like `Int()`.
