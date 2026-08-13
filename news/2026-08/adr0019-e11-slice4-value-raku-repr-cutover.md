# ADR-0019 E11 slice 4: is-deeply/is-eqv diagnostic formatter routes through the resolver

`value_raku_repr`, the "expected:"/"got:" diagnostic formatter behind
`is-deeply`/`is-eqv`, was a free function that called `native_method_0arg()`
directly — which never recognizes a user-defined `.raku` override on an
`Instance`, so the diagnostic silently fell back to a generic
stringification instead of the user's own `.raku`.

This slice converts it to a `&mut self` method routing through
`call_method_with_values()`, guarded by `e2_native_method_exists()` (from
slice 2) to preserve the exact prior fallback: an unrecognized
`(val, "raku")` pair still falls back to `to_string_value()`, not a dispatch
error. The four call sites in `test_functions/comparison.rs`'s combinator
chains were converted to closures over `&mut self`.

New pin: `t/is-deeply-user-raku-diagnostic.t` (verified against real `raku` —
the user-defined `.raku` override shows up in the diagnostic there too).
`make test` (3132 files) green.
