# ADR-0019 Phase F box F3 closed: `.^methods` now reads straight from `RAW_ROWS`

F3's job was to delete the fourteen hand-written per-type `&[&str]` method-name slices in
`builtin_type_methods.rs` (`STR_OWN`, `NUMERIC_OWN`, `LIST_METHODS`, `HASH_METHODS`, `ANY_METHODS`,
`MU_METHODS`, and the rest, plus the four owner-specific "extra tail" arrays step 2 had to add) and
the test-only `METHOD_UNIVERSE`, leaving `native_method_row.rs`'s generated `RAW_ROWS` table as the
single source both dispatch admission and `.^methods` introspection read from.

Step 2 (raku-verifying the ~90+ names `RAW_ROWS` recognizes per owner beyond what the old
introspection arrays listed, classifying each as a genuine `.^methods` gap vs. a deliberately
dispatch-only/internal name) had already closed all 18 `BUILTIN_METHOD_OWNERS` in a series of prior
sessions. Step 3, the actual cutover, needed a way to encode "this row is a real `.^methods` entry"
directly on the table instead of via a second array.

## Mechanism

Added a fourth `NativeRowFlags` bit, `INTROSPECTABLE`. A row carries it exactly when its
`(folded owner, name)` pair was a member of the old hand-written introspection arrays -- i.e.
exactly the raku-verified names step 2 confirmed. Computing which of the table's 1108 rows qualify
was mechanical (a throwaway `#[test]` diffing the live pre-deletion `builtin_type_method_names`
output against every `RAW_ROWS` row, 652 matches, zero left over on either side), but applying it
needed care: `native_method_row_table.rs` is `#[rustfmt::skip]`'d hand-frozen data with load-bearing
inline comments explaining *why* each cluster of rows was added, so a full regenerate-and-rewrite
would have destroyed that history. A small script patched the flags column of only the 652 matching
lines in place, leaving every comment and the other 456 rows untouched.

`builtin_type_methods::builtin_type_method_names` is now three lines: fold the owner via
`canonical_builtin_owner`, then read `native_method_row::introspectable_names_for_owner(folded)` --
a straight filter over `RAW_ROWS` by owner and the new flag, in table order. `RAW_ROWS`'s order for
the introspectable subset was already guaranteed to match the old arrays' order (pinned since step
1 by `raw_rows_cover_every_introspection_name_in_order`), so the cutover changes zero observable
behavior; that test now doubles as a construction-time regression guard rather than an independent
cross-check.

## Verification

`t/can-methods-drift.t` (193 assertions), the full `t/` suite (3167 files, release binary), and
every `S12-introspection/*`, `S02-types/{hash,array,list,range}.t`,
`S09-typed-arrays/hashes.t`, `S32-{str,num}/*`, `S32-container/buf.t`, and `S03-operators/buf.t`
roast file stayed green. `cargo test --lib` (826 tests) and clippy on the touched files were clean.

## Net effect

`RAW_ROWS` -- already the dispatch-admission source since box E4b -- is now also the sole
`.^methods` source. The per-type method-name lists ADR-0019's original ANALYSIS §4-1 called out as
technical debt are gone; the `test-only METHOD_UNIVERSE` constant and its now-unused
`native_responds_to` probe helper were deleted too (`builtin_sample_value`/`native_method_arities`
stay, since `native_method_row.rs`'s inverse-probe tests still use them for an unrelated concern:
confirming a row's claimed arity is actually backed by the real dispatch cascade).
