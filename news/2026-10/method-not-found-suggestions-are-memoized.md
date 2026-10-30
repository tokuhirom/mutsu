# `.?name` on a type without the method no longer recomputes "Did you mean" every call

`$x.?name` and `$x.*name` run the call and discard the `X::Method::NotFound` it
raises. Building that error walked every candidate method name of the invocant's
type with an edit-distance check to word the "Did you mean" tip: ~1.34M instructions
per miss (Red's column inflator probe, `$attr.type.?inflator`, paid it for every
`Red::Column` it built). The suggestions are a pure function of the type and method
names, so a bounded thread-local memo now answers repeats: a `.?` miss on `Int`
costs 61k instructions (was 1.34M), and the message is byte-identical. Refs #15498.
