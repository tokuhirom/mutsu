# ADR-0019 E1a: a stable TypeId and shadow-mode receiver classifier

Dispatch's "who owns this method for this receiver" decision was scattered across ~27 sites and
backed by four disagreeing builtin-MRO tables and four near-duplicate type-naming functions. E1a
lays the foundation for unifying it: a `TypeId` newtype over `Symbol` (`src/type_id.rs`), a
`WellKnownTypes` struct for O(1) comparisons against common types, a single static
`BuiltinTypeInfo` catalog (`src/builtins/builtin_type_catalog.rs`) adjudicated row-by-row against
real `raku -e 'say T.^mro; say T.^roles'` output rather than the union of the existing tables, and
one classifier — `Interpreter::receiver_dispatch_class`/`dispatch_mro`
(`src/runtime/receiver_class.rs`) — that resolves any receiver (Instance, Package, ParametricRole,
concrete builtin, Enum, role Mixin) to one ordered `TypeId` chain.

This slice is purely additive: four shadow probes compute the classifier's answer beside each
dispatch-critical site's existing string-based decision and compare-and-count under new
`MUTSU_VM_STATS` counters (`owner_shadow_checks`/`owner_shadow_mismatches`). No dispatch outcome
changed.

A `MUTSU_VM_STATS=1` sweep over the full `t/*.t` suite plus roast S02/S06/S12/S14 (~26,000 shadow
checks) found ~611 mismatches (2.3%), every one accounted for in exactly three buckets: Enum
receivers (the classifier correctly puts the enum type ahead of `Int` in the MRO chain, where the
legacy string path collapsed it to plain `"Int"`), role Mixin/ParametricRole receivers (the legacy
path reported the generic `"Any"`/`"Package"` or the pre-mixin type, ignoring the role layer
entirely — exactly the failure mode E1 exists to fix), and one `multi_arg_type_keys`
Package-collision case that couldn't be reproduced as a live bug in two attempts and was filed as
its own ticket rather than guessed at.

Two follow-up tickets came out of the investigation: `mixin-role-order-not-tracked.md` (mutsu
resolves `(0 but A) but Z`'s method collisions alphabetically since `MixinOverrides` carries no
application-order information, where raku has the later-applied role win — the classifier
deliberately mirrors the same wrong-but-deterministic behavior rather than diverging further, since
a real fix needs an order field threaded through every mixin construction site) and
`multi-arg-type-keys-package-collision.md` (the unconfirmed cache-key collision above).
