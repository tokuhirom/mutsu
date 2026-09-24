# `$s.Str[0] = 1` is refused instead of silently doing nothing

```raku
my $s = "ab"; $s.Str[0] = 1;   # raku: Cannot modify an immutable Str (ab)
```

mutsu finished without an error and left `$s` unchanged. An element store on a
method result goes through `builtin_index_assign_method_lvalue`
(`src/runtime/builtins_multidim_assign.rs`). #9208 taught it to store into a
mutable Array/Hash the method returns and to refuse an immutable `List`. A
plain non-container result (`Str`, `Int`, `Rat`, ...) still fell through to the
generic copy-and-write-back tail, which had nothing to store into and reported
success.

A plain receiver's method result is now checked with
`scalar_subscript_protocol_error`, the same check a subscript store into a `$`
holding that value already uses. So a positional store dies with
`X::Assignment::RO` ("Cannot modify an immutable Str (ab)"), and an
associative store dies with "Type Str does not support associative indexing.",
matching rakudo.

Pinned by `t/oo/method/method-result-scalar-subscript-store-refused.t` (#9256).
