# `.subst-mutate` with no match answers `Nil`

`$s.subst-mutate(...)` returned the `Any` type object when nothing matched. Rakudo answers `Nil`
(and an empty list under `:g`/`:x`), so mutsu now returns the `.match` result unchanged.
