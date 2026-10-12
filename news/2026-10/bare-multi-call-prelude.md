# A bare-name multi call stops rebuilding its package search list

Before a bare-name multi call reaches its cached dispatch plan, the resolver
works out which packages to search. It also checks whether any `multi`
candidate is in scope. Both answers were rebuilt on every call. The package
list was copied into a fresh `Vec<String>` with one `String` per enclosing
package, and each package was probed for a `proto` of the name. The candidate
check walked the base-name index.

Now the search list is the memoized package chain itself, an `Arc` of
symbols. It is truncated only when a package on the chain really owns a
`proto`, and a program with no `proto` at all skips that probe. The candidate
check uses the generation-tagged cache the call-site paths already use. A user
operator's `&infix:<op>` lookup also reads the per-name memo of its
code-variable key, instead of building it with `format!` and interning it on
every operator call.

Measured with callgrind on the profiling build:

| | before | after | change |
| --- | --- | --- | --- |
| #10110's operator repro | 756.3M Ir | 690.2M Ir | -8.7% |
| the subset/`where` section of `bench-multi-dispatch` | 889.8M Ir | 805.7M Ir | -9.5% |
