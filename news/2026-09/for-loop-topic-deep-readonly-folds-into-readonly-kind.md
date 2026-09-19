# The `for`-loop topic's deep-readonly mark is now part of `ReadonlyKind`, not a second env key

ADR-0097 §10 (the scoping investigation `claude/dreamy-fermi-uyh6p6` wrote up after finding
"slice 2" was not a uniform next step) named `deep_readonly` as dynamic, set-and-cleared-per-iteration
state — not a declaration-settled fact `CompiledCode::binding_descs` (slice 1) can hold — and left
picking it up to a future round, alongside two other candidates (designing the full "runtime half"
array parallel to `locals`, or taking `type`/`hash_key_type` as their own slice).

This round takes `deep_readonly` on its own, without building the runtime-half array §10 sketched.
Reading the actual call sites turned up a smaller, lower-risk vehicle already sitting right next to
it: `Interpreter::readonly_vars` (a `RefCell<ReadonlySet>`, `Symbol`-keyed, with proper scope-journaled
mark/unmark/restore) is *itself* the general-purpose "dynamic, per-frame, name-keyed runtime fact"
mechanism ADR-0097 is reaching for — it was simply never used for this one property, which instead
grew its own independent `__mutsu_deep_readonly::<name>` env marker, written and removed at six call
sites in `vm_for_loop_body.rs`, alongside (never as part of) the `ReadonlyKind::Immutable` mark
`readonly_vars` already carried for the very same binding.

## What changed

`ReadonlyKind` (`src/ast.rs`) gained a fourth variant, `ImmutableDeep`: the same "Cannot assign to an
immutable value" `$_ = ...` refusal as `Immutable`, plus the method-mutation refusal (`.value = ...`
on a `Pair`/`Mix`/`Set`/`Bag` item) that used to be the separate env marker's whole job. `for`-loop
topic binding (`vm_for_loop_body.rs`) now marks `"_"` with `ImmutableDeep` instead of `Immutable` +
a parallel `env.insert`, and the method-lvalue dispatch site (`assign_method_lvalue_with_values`)
checks `readonly_kind(var_name) == Some(ImmutableDeep)` instead of probing
`MetaNs::DeepReadonly.owned_key_for_str(var_name)`. `MetaNs::DeepReadonly` and the
`__mutsu_deep_readonly::` namespace are retired.

## The bug this incidentally fixes

Because the deep flag was a *second*, independent fact, `restore_topic_readonly` — which correctly
restores the saved `ReadonlyKind` on loop exit — could only restore the shallow half; the three call
sites around it always unconditionally `remove()`d the env marker regardless of what the enclosing
scope needed. A `for`-loop over an immutable `QuantHash` (`Mix`/`Set`/`Bag`) whose body ran a nested
`for`-loop over its own implicit topic (even one over a plain list) lost its own deep-readonly mark
the moment the inner loop exited and restored `"_"` — so `.value = ...` on the outer topic, reached
right after the inner loop, was wrongly allowed. Folding the flag into `ReadonlyKind` means
`saved_topic_readonly: Option<ReadonlyKind>` now captures the *whole* fact including deep-ness, and
`restore_topic_readonly`'s existing `mark_readonly_sym_with(sym, kind)` restores it correctly with no
special-casing at all — there is no longer a second thing to remember to restore. Pinned by
`t/vm/binding/for-loop-topic-deep-readonly-nesting.t`.

## Where this leaves ADR-0097

One more of the 26 name-keyed namespaces retired (25 remain). The runtime-half array §10 sketched is
still undesigned and still needed for `shaped_array_dims` (verified against `raku` to be genuinely
per-invocation, not per-declaration, so it cannot move onto the declaration-settled
`CompiledCode::binding_descs`). `type`/`hash_key_type` (150+19 call sites) and `constant_var`'s
EVAL-bareword-enumeration consumer are both still open, exactly as `claude/dreamy-fermi-uyh6p6`'s
round left them. Issue: [#8069](https://github.com/tokuhirom/mutsu/issues/8069).
