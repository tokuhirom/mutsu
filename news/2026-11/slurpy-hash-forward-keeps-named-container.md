# `*%h` slurpy hash forwarded as `|%h` keeps the caller's container of a named argument

`sub w(*%h) { root |%h }` called as `w :deps($k)` now hands an `is raw` / `is rw` named
parameter of `root` the caller's own cell, as rakudo does. The `*%h` binder shares the
`capture_named_container` mechanism of the `|c` capture (#16121), and its writeback is
recorded the same way. Closes #16181.
