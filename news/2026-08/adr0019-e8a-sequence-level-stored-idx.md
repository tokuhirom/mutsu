# ADR-0019 E8a: the candidate sequence gains structural ordering fields

Phase E's dispatch resolver (ADR-0019) starts box E8 — modeling multi/proto/submethod
ordering in the shape-independent candidate sequence introduced by E4a. E8a's slice: give
`ResolvedCandidate::User` (`src/runtime/resolution_sequence.rs`) two new fields, `level: u16`
(the candidate's position in the MRO chain it was built from, 0 = the receiver's own class) and
`stored_idx: u16` (its position within that level's stored declaration order), set from
`resolve_sequence`'s existing per-level, per-overload loop. No extra sort is needed: the
sequence's own construction order already is `(level, stored_idx)`-ascending — these fields
exist as the queryable structural facts a later box (E9's dispatch cursor) will need.

A composed role's own raw MRO-level candidate is now dropped at sequence *build* time via a new
`drop_flattened_role_duplicate_candidates` helper, mirroring the existing post-match
`drop_flattened_role_duplicates` in `resolution_method.rs` but applied earlier — a
behavior-preserving move, since the dedup only removes by owner identity and the flattened copy
that survives has the same signature as the raw one it replaces.

The signature-match filtering loop shared by E4a's winner probe and this box's own deferral
probe was extracted into `Interpreter::match_sequence_candidates`, a pure code-motion refactor
so both probes consume the same candidate-slice filter instead of each carrying a copy.

The new `Interpreter::shadow_check_deferral_sequence`, gated behind `MUTSU_VM_STATS` like every
prior Phase E probe, hooks `Interpreter::push_method_dispatch_frame` — the single real call site
that builds the `nextsame`/`callsame` "remaining" deferral list today via
`resolve_all_methods_with_owner` plus fingerprint-based winner removal. It builds the same list
from the sequence's own `(level, stored_idx)` order, filtered per-call and with the winner's
fingerprint removed the same way the real code does it, and compares the two fingerprint lists
under a new `DEFERRAL_SHADOW_CHECKS`/`_MISMATCHES` counter pair. The winner-selection half of
"shadow-compare winner and deferral list" needed no new code: E4a's existing winner probe
already covers it, since the new fields do not change ranking at all.

Building this surfaced two findings. First, a real bug in the new probe itself: an early version
passed the call's invocant into the signature match, but the real target
(`resolve_all_methods_with_owner`) always matches invocant-BLIND (it never checks `:U:`/`:D:`
smiley constraints on the deferral list, mirroring raku's own `nextsame`/`callsame` semantics) —
an invocant-aware probe was stricter than its own target and mismatched on every
`::?ROLE:U:`/`::?ROLE:D:` multi pair in the sweep. Fixed by matching invocant-blind, the same
way the real code does.

Second, a pre-existing, accepted divergence: `resolve_sequence`'s per-level lookup silently
misses a role's own methods unless the role has been *punned* at some point — the E1/E2
canonical `method_entries` table is only ever populated from `self.classes`, never directly from
`self.roles`. The real deferral walker has no such gap (it reads the role table directly), so
every remaining shadow mismatch (58 of 160 checks across 46 `t/` files, one root cause confirmed
by hand on all ten mismatching files) traced to this single, already-latent gap. Left
undisturbed rather than fixed inline — the same table also feeds several REAL production
dispatch paths including winner selection, so extending its role coverage is a genuine
behavior change outside this shadow-only box's scope — and written up in full, with a suggested
fix and verification plan, in `todo/deep/method-entries-never-covers-unpunned-roles.md`.

A `MUTSU_VM_STATS=1` sweep of the local `t/` suite (3070 files) plus a roast slice touching
multi/role/submethod/wrap dispatch (16 files, `S06-advanced`/`S06-multi`/`S12-methods`/
`S12-class`/`S14-roles`) found 37 roast checks with 0 mismatches, and 160 `t/` checks with 58
mismatches, all attributed to the documented role-coverage gap above. Two new unit tests pin
the level/stored_idx computation and the build-time role-dedup. `make test` (3070 files / 28652
tests) is green; nothing in real dispatch changed.
