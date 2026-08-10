# `multi_arg_type_keys` no longer collapses every type object to "Package"

`multi_arg_type_keys` (`src/vm/vm_call_method_compiled_cache.rs`) computes the
sound multi-dispatch resolution cache key for `resolve_method_cached` /
`resolve_function_multi_cached` — one key per positional argument, so two
calls with the same shape of argument types can safely reuse a resolved
candidate.

For any argument that was not one of the explicitly-matched `ValueView`
variants, the key fell through to `value_type_name(a)`. For a bare type
object (`Int`, `Str`, `Foo`, any class passed undefined — e.g. calling
`multi sub f(Int $x) {...}` as `f(Int)`), `value_type_name` always returns
the literal string `"Package"` regardless of which type it names. So every
type-object argument to the same multi was keyed identically, no matter its
actual type.

This was found via the ADR-0019 Phase E box E1a shadow probe
(`MUTSU_VM_STATS=1`), which showed `multi_arg_type_keys [old=Package
new=<ActualTypeName> ...]` mismatches on several `t/` files exercising
`.can`/`.does`/custom-`HOW` (method dispatch, not function dispatch — the
OTF-compiled-multi gate excludes the plain-function shape from this cache
path today). A live wrong-answer repro through the method-dispatch path was
not constructed, so this was fixed preventively rather than chasing a
symptom.

Fixed by adding an explicit `ValueView::Package(name) => name` arm, mirroring
the existing `ValueView::Instance { class_name, .. } => class_name` arm, so a
type object keys on its own name.

Verified via the shadow-probe counters: `owner_shadow_mismatches` for
`multi_arg_type_keys` drops to 0 on the four files that previously showed a
mismatch (`t/can-does.t`, `t/custom-how-type-check-writeback-coherence.t`,
`t/await-died-exception.t`, `t/augment-builtin-datetime.t`). A new Rust unit
test (`vm::vm_call_method_compiled_cache::multi_arg_type_keys_tests`) pins
that `Int` and `Str` type-object arguments now produce distinct keys.
