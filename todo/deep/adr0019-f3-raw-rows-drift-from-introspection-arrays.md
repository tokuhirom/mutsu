# ADR-0019 F3 scoping: `NativeMethodRow`'s `RAW_ROWS` has drifted from the 14 introspection arrays it was generated from — a straight cutover is not safe

F3 ("Delete the per-type method-name lists and the test-only `METHOD_UNIVERSE`... retain only the
generated native entry catalog that dispatch itself consumes") was picked as the next Phase F slice
after the F1/F3 hand-table-tension decision
(`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`'s "Decision (2026-08-14)") named it as
independent of F1 and safe to start first. Before writing any code, checked whether "the generated
native entry catalog" (`src/builtins/native_method_row.rs`'s `NativeMethodRow`/`RAW_ROWS`) can
actually replace `builtin_type_methods.rs`'s 14 `&[&str]` arrays (`STR_OWN`, `NUMERIC_OWN`, ...,
read via `builtin_type_method_names()`/`builtin_method_entries()`). It cannot, as-is.

## What was assumed vs. what's true

`native_method_row.rs`'s own doc comment says `RAW_ROWS` was "generated once (2026-08-10) by
probing every (owner, name) pair from the existing 14 built-in-method name slices... against the
real native dispatch cascades." That reads as "this table's rows are exactly those arrays' union,
plus arity/flag classification" — which would make a cutover mechanical: replace the arrays with a
"group `RAW_ROWS` by owner, preserving its own row order" function.

**That was true on 2026-08-10 and is not true now.** `RAW_ROWS` is a flat, hand-frozen data table
(`native_method_row_table.rs`) that has not been kept in lockstep with `builtin_type_methods.rs`'s
arrays as later Phase E work (mainly E2b's twelve gap-closing slices, 2026-08-10) added names to
`RAW_ROWS` for *dispatch-recognition coverage* without also adding them to the introspection arrays
— because E2b's own job was "drive `native_call_unmodeled` toward zero," which only cares whether a
`(owner, name)` pair the *real dispatch cascades* accept has a matching row, not whether that name
also belongs in `.^methods`'s public list. `native_method_rows()` (the `#[cfg(test)]` per-owner
enumeration used by the one existing cross-check, `native_method_rows_matches_builtin_entry_count`)
does not actually enumerate independently — it calls `builtin_method_entries(type_name)` for the
`(owner, name, order)` triples and only uses `RAW_ROWS`/`classification_table()` for the arity/flags
*lookup* at each of those already-known keys. It cannot answer "what does owner X's `RAW_ROWS` slice
alone contain" — nothing outside `#[cfg(test)]` needs to ask that question today, so nothing does.

## The actual diff (ad hoc probe, 2026-08-14, not committed)

Filtering `RAW_ROWS` by owner (preserving its own literal order) and diffing against
`builtin_method_entries(owner)` for all 18 `BUILTIN_METHOD_OWNERS` (`Blob` included; `FatRat`/
`Method`/`Block`/`Routine` are aliases folded by `canonical_builtin_owner` so not checked
separately) found mismatches on every owner except none — every single owner differs:

- **`Sub` has ZERO rows in `RAW_ROWS`** — E2a/E2b's probe never covered it (the design doc's own
  E1a notes list `Sub`/`Signature`/`IO::Path`/`IO::Handle`/`Cool` as owners the initial probe
  skipped). `builtin_method_entries("Sub")` has 10 names (`name`, `signature`, `arity`, `count`,
  `of`, `returns`, `Bool`, `Str`, `gist`, `raku`). A cutover reading owner enumeration from
  `RAW_ROWS` would silently make `Sub.^methods`/`.^can` (and therefore `can-ok`, `.^lookup`, and
  registry seeding) report **no methods at all** for `Sub`/`Method`/`Block`/`Routine`/`Code`.
- **Most owners have dozens of EXTRA `RAW_ROWS` entries** the introspection arrays don't list —
  e.g. `Any` has `self`, `clone`, `serial`, `hash`, `WHICH`, `sink`, `item` (7 extra, one of which,
  `WHICH`, IS listed but on `Mu` instead — see below); `Str` has 25 extras (`uniprop`, `AST`,
  `indent`, `list`, `UInt`, `FatRat`, `sprintf`, `ord`, `uniname`, `uninames`, `unival`, `univals`,
  `chrs`, `bytes`, `tclc`, `Range`, `Complex`, `Version`, `Real`, `Date`, `DateTime`, `reverse`,
  `byte`, `perl`, plus one duplicate-looking near-miss); `Int` has 25 extras including `rand`,
  `Array`, `Supply`, `pairs`, `AT-KEY`; `Cool` has 11 extras, all typed-int/uint aliases (`int8`
  .. `uint64`, `byte`, `int`, `uint`); `Hash` has 11 extras (`pick`, `EXISTS-KEY`, `AT-KEY`, `List`,
  `invert`, `flat`, `Array`, `AT-POS`, `EXISTS-POS`, `dynamic`, `roll`, `perl`). Some of these look
  like genuinely-dispatchable methods `.^methods` deliberately omits (implementation-detail
  protocol methods like `AT-POS`/`AT-KEY`/`EXISTS-POS`/`EXISTS-KEY`, or Rakudo-internal ones like
  `serial`/`sink`/`item`); others may be real gaps in the introspection arrays. Telling these apart
  needs a raku ground-truth check per name, not a blind union.
- **Order differs, not just membership**: `Signature`'s `RAW_ROWS` slice is `["gist", "raku",
  "params", "arity", "count", "returns", "Bool", "Str"]` vs. the array's `["params", "arity",
  "count", "returns", "Bool", "Str", "gist", "raku"]` — same 8 names, different sequence. A blind
  "enumerate from `RAW_ROWS`'s own order" cutover would silently reorder `Signature.^methods`
  output (order is asserted by roast/`t/` introspection tests elsewhere in the codebase).
- **`Mu` has one extra**: `DEFINITE` (a real Rakudo `Mu` method raku exposes; plausibly a genuine
  gap in `MU_METHODS`, not dispatch-only noise — worth checking against raku on its own).

## What this means for F3

The box's framing ("retain only the generated native entry catalog that dispatch itself consumes")
assumed that catalog already IS a superset-with-order-info of the introspection arrays. It is
neither superset (missing `Sub` entirely) nor order-preserving (`Signature`) nor free of entries
that may or may not belong in public `.^methods` output (the ~90 extra names across other owners).
**A direct "point `builtin_type_method_names`/`builtin_method_entries` at `RAW_ROWS` instead"
cutover is not safe** — it would silently change `.^methods`/`.^can`/dispatch-registration output
for every one of the 18 owners, not a behavior-neutral refactor.

F3 needs a real design/verification pass before code, the same way F1 did — NOT a "the box says
plumbing, so this is 30 minutes of grep-and-replace" mechanical slice. Suggested shape for that pass
(not started):

1. **Regenerate `RAW_ROWS` from the current arrays first**, the same way it was generated
   originally (a throwaway `#[test]` probe pasted as data, per the module's own doc comment
   discipline), so the "missing `Sub`" and any staleness from array edits since 2026-08-10 are
   closed. This alone re-establishes the invariant F3 was assuming.
2. **Triage the ~90+ extra `RAW_ROWS` names** (dispatch-recognized but not in the introspection
   arrays) into: (a) genuine `.^methods` gaps — verify against real `raku`, add to the relevant
   array/owner if raku lists them; (b) deliberately-internal/protocol methods (`AT-POS`, `EXISTS-KEY`,
   `sink`, `item`, `self`, `clone`, `serial`, ...) that dispatch recognizes but `.^methods` correctly
   omits — these need an explicit "dispatch-only, not introspectable" flag/list surviving the
   cutover, not silent inclusion.
3. **Only then** can `builtin_method_entries`/`builtin_type_method_names` safely read from the
   (now-reconciled, now-complete, now-order-matching) single catalog and the 14 arrays actually be
   deleted.

Given the size of step 2 (raku-verifying ~90+ names across 18 owners), this is closer in shape to
F1's own "dedicated raku ground-truth session" than to a mechanical plumbing change. Filed here so
the finding isn't lost; not started. Suggest treating this as its own dedicated slice/session rather
than folding into whatever slice picks F3 up next.
