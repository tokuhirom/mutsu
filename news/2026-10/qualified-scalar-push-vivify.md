# `$GLOBAL::n.push(1)` from a routine survives the frame, and a vivified scalar is itemized

Calling `.push`/`.append`/`.unshift`/`.prepend` on an unset scalar
auto-vivifies it to an Array. For a package-qualified name such as
`$GLOBAL::n`, the vivified array was written only to the running frame's env.
The frame's exit then dropped it, so `sub f { $GLOBAL::n.push(1) }; f()` left
`$GLOBAL::n` as `Any` (#10620). It is now also persisted in `our_vars`, the
same way `SetGlobal` and the qualified `++` (#10493) already persist theirs.
The env entry and the persisted one share one array, so the push lands in
both.

The vivified array also now sits in the scalar's container, as in Rakudo:
`my $x; $x.push(1); $x.raku` is `$[1]`, not `[1]`. That holds for every `$`
variable, not only package-qualified ones.

The array-sigil twin, `@GLOBAL::b.push(1)` from a routine, needs a read-side
fallback as well. It is filed as #10800.
