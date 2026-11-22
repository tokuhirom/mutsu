# A named capture parameter takes traits

`sub f(|c is raw) { ... }` failed to compile, and the signature literal
`:(|c is raw)` came out with no parameters (#18684). The parser read traits
after an anonymous capture (`| is raw`) but not after a named one. A named
capture now takes `is <trait>` before its `where` clause too.

A sigilless parameter (`\x`) is raw by construction, so its `.raku` no longer
renders an explicit `is raw`: `:(\x is raw).raku` is `:(\x)`, as in Rakudo.
