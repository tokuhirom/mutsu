# Test::Stream: block-scoped method state persists, plus `nqp::time_n`, `&Pkg::infix:<op>` and exception `eqv`

Running Test::Stream's own suite under mutsu exposed several interpreter gaps, now fixed:

- A method declared in a bare block of a class body (`class C { { my $i; method m { $i //= ... } } }`) re-installed its captured lexicals on every call, so a write never outlived the call. Writes are now stored back into the method's capture and shared with the sibling methods of the same block. A typed block lexical also carries its `__mutsu_type::` constraint with it, so a same-named typed variable in the caller no longer constrains it.
- `nqp::time_n` and `nqp::time_i` (fractional and whole seconds since the epoch).
- `&Pkg::infix:<op>` (and the other operator categories) parses as a package-qualified operator reference.
- `EVAL '&infix:<not-an-op>'` throws `X::Undeclared::Symbols` for an undeclared word infix instead of returning a stub routine.
- `eqv` on two built-in exception objects compares what their `.raku` renders, so a thrown exception equals a constructed one.

Five of the six Test::Stream test files now pass; `t/200-event-source.rakutest` still needs #12266 (`callframe` through the light call paths).
