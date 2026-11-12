# A module's private sub is called through the call cache

A package-less top-level `sub` of a loaded module is lexical to that module's
compilation unit, so the same bare name can mean a different routine in each
unit. mutsu's call caches were keyed by name and callsite package only, so
every call to such a routine skipped them all and re-ran the whole name
resolution walk -- `has_declared_function`, `user_function_matches_call`, the
builtin-shadowing checks and the `Pkg::name` key probes, about 15k
instructions before the callee's frame was even pushed.

The on-the-fly call cache's entry now records the unit a compunit-private
routine was resolved in, and a call from that same unit is answered from it;
every other reason a name skips the name caches (a `&name` parameter
shadowing it, a `sub EXPORT` override, an import into an open unit scope)
still keeps it off. The cache's hit also no longer refuses a body that
creates a closure: its general arm makes the same call the uncached path
makes.

Monad's type helpers (`some-type`, `parameterized`, `absent`) are such
subs. `Monad::Maybe.some(1)` drops from ~395k to ~312k instructions, and a
single-threaded 1500-chain run of `t/14_concurrent_types.t` from 11.0 s to
9.4 s (#17331).
