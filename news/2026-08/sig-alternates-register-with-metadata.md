# Signature alternates register with per-slot plan metadata

An S13 multi with alternate signatures (`multi sub f($a) | ($a, $b) {...}`)
registers one candidate per signature, all sharing the declared body. The
primary candidate has registered from plan-lowered `CompiledRoutineMetadata`
since ADR-0019 C6e-3a — fingerprints, body facts, and effective param defs
seeded eagerly so no code path needs to re-walk the AST body — but the
alternates went through `register_sub_alternate_decl` with no metadata at
all, leaving their fingerprint/facts caches to a lazy walk over the plan
body. Since C6e-3b registers safe-class defs with an *empty* body, that lazy
walk was hashing nothing: alternate candidates of a body-less plan carried
colliding identity values.

The plan now lowers a metadata record per `signature_alternates` slot
(`CompiledSubDeclPlan::alternate_metadata`, index-aligned; the shared
`compiled_routine_metadata` helper computes the signature-derived fields —
effective param defs, `registration_identity`, `body_fingerprint`,
`empty_sig` — per slot over the shared body), and the registration op hands
each alternate its slot's metadata. The C6e-3a debug asserts (seed == lazy
while a body is still attached) now cover the per-slot values too, with the
whole `t/` suite running on the debug binary in CI.

This clears the "per-slot metadata for signature alternates" blocker from
the C6e-3c cut-line (`todo/deep/c6e-legacy-body-drop-blocked-by-gate-
rejected-shapes.md`); the field drop now waits only on the remaining
keep-classes (unresolvable plan bytecode, routine-level lvalue forms,
NativeCall traits).
