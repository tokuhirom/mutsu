# ADR-0019 F6 mut-dispatch family tagged; fixes a real E4 sequence-resolver bug for role puns

ADR-0019 F6 ("delete compatibility call carriers and dead resolver modules") requires every
`run_instance_method` caller family to first be tagged with a `run_instance_method_at` `site` and
shadow-checked against the modern `resolution_sequence::resolve_sequence` resolver before it can be
migrated off the carrier. The mut-dispatch family (`methods_mut_dispatch.rs`'s native-lever-A mirror
at `call_method_mut_with_values`'s top and the general mut-dispatch fallback near its end, two sites)
is now tagged `"mutdispatch"`, following the same additive pattern already used for the coercion,
mut-lvalue, qualified-dispatch, and instance-ops families.

Gathering the corpus evidence for this tag (a full local `t/` sweep, `MUTSU_VM_STATS=1`) surfaced a
genuine mismatch in `t/role-bless-pun.t`: `Service.bless(...)` on a bare role's `.start`/`.running`
methods resolved correctly via the ad-hoc `run_instance_method` walk but came back empty from
`resolve_sequence`.

**Root cause:** `resolve_sequence`'s `drop_flattened_role_duplicate_candidates` step removes a raw
role-level method candidate whenever some candidate in the sequence carries a matching
`role_origin` — meant to drop a role's un-flattened copy once a *differently-owned* class-level
level already carries the flattened copy (`class Foo does R`: `Foo`'s copy has
`role_origin = Some("R")`, which correctly marks the separate `R`-owned MRO level as redundant).
But a role **pun** (`Service.bless`/`.new` on a bare role) copies the role's own methods into a
synthetic class registered under the role's own name, tagging each copy `role_origin =
Some(role_name)` — the same name as the copy's own owner. The old filter didn't distinguish this
self-reference from a genuine cross-owner duplicate, so a pun's sole MRO level for that method
matched its own `role_origin` and was deleted outright, leaving `resolve_sequence` with nothing.

The fix only adds a `role_origin` to the "flattened" set when it names an owner *different* from
the candidate carrying it — matching `resolve_method_with_owner_impl` (the real dispatch resolver
this sequence exists to reproduce), which never drops a pun's own single-level candidate.

This also closes `todo/tickets/adr0019-e4-sequence-resolver-misses-type-object-dispatch.md`: all 9
of that ticket's previously-recorded mismatches (`NotNewPun.x`, `Elsewhere::Header.tag`, a
`role R { multi method COERCE {...} }` type-object call, etc.) were the same self-referential-pun
shape, just reached through a type-object receiver instead of a bless'd instance. A full local `t/`
sweep after the fix (3189 files, `MUTSU_VM_STATS=1`) finds zero `"mutdispatch"` or `"instanceops"`
mismatches — the only two remaining corpus-wide are the pre-existing, unrelated `"privatedispatch"`
pair already documented in earlier F6/E7 progress notes.

As a side effect, the fix also makes the VM's cached fast dispatch path (`resolve_method_cached` ->
`resolve_via_sequence_cache`, which reads `resolve_sequence`) able to resolve a role pun's methods
directly instead of always missing and falling through to the slow `run_instance_method` path —
observable in `t/role-bless-pun.t`, where the tagged `"mutdispatch"` call sites stopped firing at
all once the cached path started succeeding on its own.

Verified: full local `t/` suite (3189 files) green, `cargo clippy -- -D warnings` / `cargo fmt`
clean, the `S04`/`S06`/`S09`/`S12`/`S14` whitelisted roast subset (309 files, release) green, and
`scripts/battery-testsuite.sh` GATE PASSED.
