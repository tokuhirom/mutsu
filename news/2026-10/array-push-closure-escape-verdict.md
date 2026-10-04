# `Array.push` preserves a stored closure's escape verdict

The compiler's dedicated `@array.push(single_expr)` path marked a pushed
closure literal as non-escaping, although the array retains it after the call.
A closure that read a lexical reassigned by its creating routine could then
miss the shared capture cell. When invoked later from a frame with a
same-named lexical, it read that unrelated caller value.

The fast path now applies the ordinary method-call rule to closure arguments.
In the pinned case, three closures pushed in an inlined `for` body all read
their owner's later value `42`, including when called from a frame whose own
`$shared` is `99`. A second invocation keeps an independent binding.

This settles the `for`/`push` example in ADR-0055 §7.4. The other cell-coverage
routes there remain prerequisites for the closure-wins merge policy.
