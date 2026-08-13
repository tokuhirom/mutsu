# ADR-0019 E11 slice 1: collapse duplicated native-arity dispatch call sites

ADR-0019's Phase E box E11 retires direct callers of the
`native_method_{0,1,2}arg` arity cascades everywhere except the resolver's own
two canonical invocation points (`Interpreter::call_method_with_values`'s
by-arity match and the VM's `Interpreter::try_native_method`) plus
`builtins/`-internal recursion and test code, so that the native arity
functions become purely handler implementations selected by `MethodEntry`
rather than something arbitrary call sites reach for directly.

This first slice found eight call sites in `src/runtime/` that manually
duplicated the exact "try native by arity, fall through" sequence
`call_method_with_values` already performs internally, then collapsed each to
a plain recursive call into that resolver:

- `methods_instance_ops.rs`'s numeric-bridge (`.Bridge`/`.Real`) delegation
  path, which re-dispatched a coerced native value through a hand-rolled
  0/1/2-arg match before falling back to `call_method_with_values` anyway —
  the match was dead weight, since `call_method_with_values` tries the same
  native cascade first.
- Six `SetHash`/`BagHash`/`MixHash` `.grab`/`.grabpairs` sites and one
  `pick`/`roll`/`grab`/`grabpairs`/`pickpairs` site in
  `methods_call_dispatch.rs`, all of which computed a Callable argument's
  `.elems`/`.total`/`.Int` input via a direct `native_method_0arg` call with a
  manual `Ok(0)` fallback instead of going through the resolver.

The receiver at every one of these sites is always a native
Set/Bag/Mix/numeric value (guarded by an outer `matches!` on `target.view()`
or reached only after a successful `.Numeric`/`.Bridge` coercion), so
`call_method_with_values`'s own native-first path serves an identical result
— this is a pure simplification, not a behavior change. Verified with
`roast/S02-types/{set,sethash,bag,baghash,mix,mixhash}.t`,
`roast/S17-supply/grab.t`, the local `does Real`/`Bridge` coercion suite, and a
full `make test` run.

A `MUTSU_VM_STATS=1` sweep of the local `t/` suite also re-confirmed that
E7 step 4's previously-deferred `.^can` cutover (native dummy-arg probe →
`Interpreter::e2_native_method_exists`) is still not safe: the E2 native-method-row
catalog has zero rows for `IO::Path`/`IO::Handle`/`Cool`/`Sub`/`Signature`/`Any`/`Mu`
because `builtin_type_methods::builtin_sample_value` has no sample value branch
for those owners, so the one-time probe that generated the catalog never ran
against them. That gap — plus `Cancellation`-shaped native-Instance methods
sitting entirely outside the arity-cascade model — is recorded in the ADR as
the next E11 sub-slice.
