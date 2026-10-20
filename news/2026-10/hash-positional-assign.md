# A positional write on a Hash follows `Any.ASSIGN-POS`

`%h[0] = 42` used to store the key `"0"` in the hash. A Hash does not do `Positional`, so
Rakudo treats index 0 as the hash itself (`%h = 42`, which dies with
`X::Hash::Store::OddNumber` for an odd list and replaces the contents otherwise), any other
index as `X::OutOfRange`, and `%h[0] := 42` as `X::Bind`. mutsu now does the same for a plain
Hash held in a `%` variable or a scalar (#13611).
