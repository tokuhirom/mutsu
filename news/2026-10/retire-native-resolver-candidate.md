# The resolver's `Native` candidate and the recognition table's arity reader are gone (ADR-11276, slice 4/5)

`ResolvedCandidate::Native`, `NativeCallShape`, `CallShape` and `native_row_servable`
existed so that the candidate sequence could say "a native arity-cascade row serves this
call". Nothing in production read the candidate (winner selection ranks `User` candidates
only; native bases are reached through `NativeBase`), and the cascades it described are
gone, so the sequence no longer depends on the call's arity and `resolved_seq_cache` is
keyed by `(owner, method)`. The arity reader of the recognition table
(`native_method_row`, `NativeArityMask`) and the inverse probes that pinned its rows to the
cascades (about 1,200 lines of tests) go with them. The table keeps what `.^can`,
`.^methods` and `augment` read: existence and the `DECLARED`/`INTROSPECTABLE`/`ONLY_METHOD`/
`REVISION_E` bits.
