# `try`'s implicit `use fatal` no longer leaks into called routines

A `try` block behaves like `use fatal`, but in rakudo only lexically: a Failure
that a routine *called* from the try stores in a variable or sinks stays a soft
Failure. mutsu turned `try`'s marking on through the dynamic `fatal_mode`
flag, so every routine called inside a try exploded on such a store. For
example, `sub n { my $x = "Inf".Int; 1 }; try { n() }` set `$!`, where rakudo
leaves it undefined. Math::NIntegrate hit this through grammar actions that
store `$<number>.Int` (a Failure for `Inf`) while running inside a
`try { ... .parse(...) }`.

The Failure explosion checks (the store-time checks, the sunk-list check and
the composite/call-argument checks) now all read `lexical_fatal_mode`. A
genuine `try` sets that channel as well, and every call entry resets it to the
callee's own compile-time state. For methods this is a new
`CompiledCode::method_fatal_pragma`, and for closures it is the value captured
when the closure is built. `EVAL` resets it too, just as it resets
`fatal_mode`. The dynamic `fatal_mode` stays in place for the deferred
`.map`/`.grep` Seq capture, which really does reach into called routines.
Pinned by `t/exceptions/try-fatal-is-lexical.t` (#11391).
