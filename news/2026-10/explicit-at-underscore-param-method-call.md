# An explicit `@_` parameter survives a `+@` method call

A sub declared `sub f(@_)` had its `@_` replaced by the callee's argument list after
calling a method whose signature has a `+@a` slurpy, which also demoted the caller's
array. The method-exit env merge treated `_` as frame-local but not `@_`/`%_`; both are
now excluded (#12485). This unblocks the `immutable` ecosystem dist's `01-basic` test.
