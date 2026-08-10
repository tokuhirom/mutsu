# ADR-0019 E4a: the sequence builder lands in shadow mode

Phase E's dispatch resolver (ADR-0019) now has its first slice of box E4 — the
one-MRO-walk candidate sequence that will eventually unify native and user
method resolution. E4a builds only the user-candidate half, purely for
shadow-mode verification: `Interpreter::resolve_sequence`
(`src/runtime/resolution_sequence.rs`) walks E1's `TypeId` receiver chain and
collects every visible user-declared method candidate per level into a flat
`ResolvedSequence`, the shape-independent candidate universe the design doc
calls for.

To rank a shadow sequence with the exact same rules the real resolver uses,
`resolve_method_with_owner_impl`'s tie-break ladder (type-hierarchy distance,
`is default`, narrowness, explicit-named preference, most-derived-owner,
`X::Multi::Ambiguous`) was extracted verbatim into a new
`Interpreter::pick_method_winner` — a pure code-motion refactor with no
behavior change, shared by both the production resolver and the new shadow
path.

`Interpreter::shadow_check_resolver`, gated behind `MUTSU_VM_STATS`, builds
the sequence at `resolve_method_cached`'s two resolution boundaries, filters
candidates through the existing `method_args_match_for_invocant`, ranks the
survivors with `pick_method_winner`, and compares the winner against the real
resolver's answer under new `resolver_shadow_checks`/`resolver_shadow_mismatches`
counters. Two guards keep this a true zero-behavior-change probe:
`dispatch_ambiguous` is saved and restored around the shadow ranking (it can
be set by `pick_method_winner`, and the caller reads it immediately after the
real resolve), and any candidate carrying a `where`-clause parameter is
skipped entirely, since a `where` clause is user code whose dynamic-variable
writes are a deliberately-preserved side effect that must not run twice.

A sweep of `MUTSU_VM_STATS=1` over the full `t/` suite (2996 files, 12396
shadow checks) plus the whitelisted `roast/{S12,S14,S32}-*` corpus (382
files, 12767 checks) found 3 mismatches total (0.012%), all in `t/`, all one
explained bucket: the real resolver returns a non-multi method's sole
candidate by name alone even when its signature does not bind the call's
arguments (an `is rw` parameter fed a literal, a role-typed parameter fed the
wrong type) — Raku raises the mismatch as a bind-time error rather than
falling through to a different candidate. The shadow builder, which only
ranks args-matching candidates, has no notion of this yet; it is the same
early-stopping rule box E8 is scoped to model, not a new finding — following
the precedent E1a set of landing a shadow-verified box with an explained-
mismatch ledger.

`make test` (2996 files / 28149 tests) and the full whitelisted roast sweep
were run and are green with this change.
