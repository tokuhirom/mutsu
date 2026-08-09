# `multi_arg_type_keys` keys every type-object argument as the literal string "Package"

Found via ADR-0019 Phase E box E1a's shadow probe at `Interpreter::multi_arg_type_keys`
(`src/vm/vm_call_method_compiled_cache.rs`), which computes the sound multi-dispatch
resolution cache key for `resolve_method_cached`/`resolve_function_multi_cached`.

## The finding

For any argument that is NOT one of the explicitly-matched `ValueView` variants
(`Instance`, `Junction`, `Mixin`, `Scalar`, `ContainerRef`, `Pair`, `ValuePair`,
`Capture`, `VarRef`), the cache key falls through to:

```rust
_ => crate::symbol::Symbol::intern(crate::runtime::utils::value_type_name(a)),
```

`value_type_name(&Value::package(...))` always returns the literal string `"Package"`
(`src/runtime/utils/type_misc.rs:93`) — the SAME string for every type object,
regardless of which type it names. So a bare type-object argument (`Int`, `Str`,
`Foo`, any class passed undefined, e.g. `multi sub f(Int $x) {...}` called as `f(Int)`)
is keyed identically to every OTHER type-object argument passed to the same multi.

Confirmed via the ADR-0019 E1a shadow counters (`MUTSU_VM_STATS=1`) sweeping `t/*.t`:
multiple files (`t/can-does.t`, `t/custom-how-type-check-writeback-coherence.t`,
`t/await-died-exception.t`, `t/augment-builtin-datetime.t`) show
`multi_arg_type_keys [old=Package new=<ActualTypeName> ...]` mismatches — i.e. the
classifier correctly resolves the argument's actual type while the existing cache-key
logic collapses it to the generic string.

## Risk (unconfirmed as a live bug)

If a multi is `multi_dispatch_type_cacheable` (no `:D`/`:U`/`where`/literal/subset
constraints on the relevant candidates — see `multi_dispatch_type_cacheable`) and is
reached through an OTF-compiled/cached call path, two calls to the same multi with
DIFFERENT type-object arguments would compute the SAME cache key and the second call
would incorrectly reuse the first call's resolved candidate.

**I could not reproduce a wrong answer in two direct attempts** (`multi sub f(Int $x)`
/ `multi sub f(Str $x)` called as `f(Int)` then `f(Str)`, both as plain functions and
via user classes) — `MUTSU_VM_STATS=1` showed `function-full-resolve` running on every
call (`resolve_function_with_types`, NOT the cached path) and `owner_shadow_checks=0`,
meaning `resolve_function_multi_cached`/`multi_arg_type_keys` was never reached at all
for that shape — the OTF-compiled-multi gate (`def_is_otf_compilable_multi_candidate`
in `src/vm/vm_call_func_ops.rs`) apparently excludes it. The real shadow-probe hits
above came from `resolve_method_cached` (method dispatch, not function dispatch) in
files exercising `.can`/`.does`/custom-`HOW` scenarios — reproducing a wrong-answer
repro through THAT path (rather than just the cache-key computation) is left to
whoever picks this ticket up.

## Fix sketch

Add an explicit `ValueView::Package(name) => name` arm to `multi_arg_type_keys` (mirror
the existing `ValueView::Instance { class_name, .. } => class_name` arm) so a type
object keys on its OWN name, not the literal "Package". This is a plain correctness fix
to the cache-key computation, independent of ADR-0019 Phase E's TypeId work — E1a
itself makes no dispatch decision, so it cannot carry this fix (E1a is explicitly
zero-behavior-change); this is a good candidate for a small, standalone follow-up PR
once someone confirms (or writes) a repro that shows the wrong answer end-to-end.

## Repro for the shadow-probe finding itself

```
$ cargo build
$ MUTSU_VM_STATS=1 target/debug/mutsu t/can-does.t 2>&1 >/dev/null | grep 'adr0019-e1a'
[mutsu vm-stats] adr0019-e1a: owner_shadow_checks=... owner_shadow_mismatches=...
[mutsu vm-stats] adr0019-e1a owner-shadow mismatches by site (top N): multi_arg_type_keys [old=Package new=Greeter ...]=... ...
```
