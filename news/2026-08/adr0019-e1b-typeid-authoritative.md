# ADR-0019 E1b: the TypeId classifier becomes authoritative

Building on E1a's shadow-mode classifier, the `TypeId`-based receiver-owner decision now actually
drives dispatch at the three sites E1a's sweep showed were safe to cut over unconditionally:
`call_method_with_values`'s augment gate, `dispatch_instance_and_fallback`'s value-type dispatch
pick, and the small fallback-arm trio in `are_actual_type_name`/`are_value_matches_type`/
`.^add_fallback`. Two new helpers do the work: `Interpreter::dispatch_owner_chain`/
`dispatch_owner_name` (`src/runtime/receiver_class.rs`), a `dispatch_mro` variant with one
deliberate difference — for a role Mixin receiver it skips the role-`TypeId` chain prefix and
returns the inner value's own chain instead.

That skip isn't optional. Every call site consulting this chain runs strictly after a dedicated,
role-registry-aware path has already tried role methods for the same receiver
(`dispatch_mixin_method_call` before the augment gate; `dispatch_qualified_mixin_method` before
qualified dispatch). Re-deriving a role owner at this point would at best repeat that lookup and
at worst regress: a direct repro confirmed it before landing — `augment class Array { method
my-foo {...} }; (@a but R).my-foo` resolved fine under the old `value_type_name`-based owner
(which unwraps a Mixin to its inner value the same way this skip does); switching to the raw
role-first chain made it unresolvable, since the role `R` has no `augment`-recorded method by that
name.

Two related sites were widened from a single owner name to walking the *whole* chain:
`methods_qualified.rs`'s qualified-dispatch membership check and `type_matching.rs`'s
`type_matches_value`. This is required, not just more thorough — an Enum value's chain is
`[EnumType, Int, Cool, Any, Mu]`, and a plain `Int` type constraint must still match an enum value
through the `Int` link further down the chain; checking only the chain's head would have broken
enum values against `Int`-typed parameters.

Two of the four remaining divergent builtin-MRO tables from the E1 design doc's original inventory
— `type_inherits`/`builtin_type_mro_chain` in `methods_call_helpers.rs` — were deleted outright,
with their two call sites now reading the classifier's own chain. A third site
(`try_compiled_method_or_interpret_inner`'s `class_sym`) needed no code change at all: it was
already provably classifier-equivalent by construction, confirmed by zero mismatches across E1a's
entire sweep, so its now-redundant shadow probe was simply removed rather than adding a
chain-walk allocation to that hot fast-dispatch path for no behavioral benefit.

One site was deliberately left on the shadow probe: `multi_arg_type_keys`
(`vm_call_method_compiled_cache.rs`). Unlike the other three original E1a sites, making it
authoritative there isn't a shadow-mode-safe refactor — it's the actual fix for
`todo/tickets/multi-arg-type-keys-package-collision.md`, an unconfirmed but plausible cache-key
collision. Bundling an unverified behavior change into this switch would have made the ticket's
own investigation (confirm or refute the collision) inseparable from this slice's zero-risk
cutover, so it stays deferred to be picked up on its own.

MOP fallback consolidation (E1c — collapsing the 13+8 per-MOP-entry owner-fallback arms into one
classifier-backed helper) remains out of scope. Verified via `make test` (28,121 tests) and a full
`make roast` (218,774 tests), both green with no regressions.
