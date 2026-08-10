# ADR-0019 E1c: MOP fallback consolidation

Phase E's receiver-owner box (E1) closes out: E1a introduced the `TypeId`/classifier in
shadow mode, E1b made it authoritative at the dispatch-critical sites, and this slice (E1c)
finishes the sweep by collapsing the remaining "who owns this MOP entry" duplication.

Twenty-two call sites across six `runtime/methods_classhow_*.rs` modules each re-derived a
receiver's owner name by hand with the same shape: a type object or instance reports its own
name, and everything else falls back to `value_type_name`, the pre-ADR-0019 string-based
classifier that flattens Enum receivers to `"Int"` and role mixins to their un-mixed base
type. Every one of those fallback arms now calls one new helper,
`Interpreter::mop_receiver_owner`, which delegates the fallback case to `dispatch_owner_name`
(the classifier E1b already made authoritative for dispatch). Four functions needed a
`&self` -> `&mut self` promotion to make the call, since the classifier caches a registry MRO
lookup; every caller of those four already held `&mut self`, so the promotion did not
propagate any further.

Two sites (`.^parameterize`'s base-type lookup and `.^roles`'s Mixin-receiver fallback)
previously had no `Instance` arm at all, silently falling to `value_type_name`'s "Any" answer
for a receiver that is never expected to be an Instance in practice. `mop_receiver_owner`
resolves an Instance there too now, consistent with the owner rules E1b already established —
a deliberate, no-known-test-affects-it broadening rather than a behavior-preserving refactor at
those two spots.

Verified via `make test` (2994 files / 28,129 tests) and a full `make roast` (1435 files /
218,748 tests), both green.
