# Binding failures at a static call site are reported inside the sub

A sub call whose argument types are all known at compile time (`g(3)`) took
the static-site path. That path is for rakudo's compile-time refutation:
"Calling f(Str) will never work". Every binding failure at such a site skipped
the callee's frame, including a `where` constraint (`sub g($x where * > 5)`).
That constraint is a run-time check in rakudo, which reports it `in sub g`.
The callee frame is now left out only for the refutation itself.

A missing required named argument on the light call path (`h(|%n)` against
`sub h(:$x!)`) did not go through the binding-error enhancement at all. It now
does, and is reported `in sub h` as well.
