# ADR-0019 F3 step 1: verified RAW_ROWS covers every introspection name, in order

ADR-0019's Phase F box F3 ("delete the per-type method-name lists ... retain only the generated
native entry catalog") was scoped in `todo/deep/adr0019-f3-raw-rows-drift-from-introspection-arrays.md`
as blocked on a raku-verification pass, because an ad hoc diff claimed the target catalog
(`native_method_row.rs`'s `RAW_ROWS`) had drifted from the 14 `builtin_type_methods.rs` name arrays
since its 2026-08-10 generation — most alarmingly, that owner `Sub` had zero rows at all, which would
make a naive cutover silently empty out `Sub.^methods`.

Re-checked with real code instead of the earlier ad hoc grep. The "`Sub` has zero rows" claim turned
out to be a probe-script bug: it filtered `RAW_ROWS` by the literal string `"Sub"`, but both
`RAW_ROWS` and `builtin_method_entries` key `Sub`/`Method`/`Block`/`Routine`/`Code` under the *folded*
owner `"Code"` (`canonical_builtin_owner`). Once folding is applied consistently on both sides, `Sub`
already has all 10 expected rows, correctly ordered. A new permanent test,
`raw_rows_cover_every_introspection_name_in_order` (`src/builtins/native_method_row.rs`), checks this
for real across all 18 `BUILTIN_METHOD_OWNERS`: every introspection-array name has a matching
`RAW_ROWS` entry, and the relative order of shared names matches. Result: **zero missing names for
every owner** — the previous 5-owner length-only check (`native_method_rows_matches_builtin_entry_count`)
is now a genuine, order-sensitive, 18-owner coverage guard.

Two owners' order genuinely did diverge from the introspection arrays — `Signature` and `Any` — because
their rows had been scattered into unrelated hand-added blocks by earlier E2b slices. Fixed by moving
the misplaced rows to the position their introspection array implies (`native_method_row_table.rs`);
no row content changed, only position.

What remains open for F3: the ~90+ extra dispatch-recognized names per owner beyond what the
introspection arrays list still need per-name raku verification (genuine `.^methods` gap vs.
deliberately-internal/protocol method) before the actual array-deletion cutover can happen safely —
see the todo file's updated "Progress" section.
