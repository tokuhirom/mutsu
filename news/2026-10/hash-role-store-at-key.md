# A role's STORE_AT_KEY sees the initializer of a role-mixed Hash

`my %h does R = ...` now applies the role before the initializer, and the
mixed container's native `STORE` stores each pair through the role's
`STORE_AT_KEY`, as Rakudo's `Hash.STORE` does. `nextwith` from
`STORE_AT_KEY`/`BIND-KEY` reaches the native Hash, `DELETE-KEY` returns the
removed value, and `my %h does R[&f] = ...` no longer reads the `=` as an
element assignment on the role. Found via the `Hash-with` distribution, whose
single test file now passes 23/23.
