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

## Progress (2026-08-14): step 1 done, and the "`Sub` has ZERO rows" claim above was a probe artifact

Ran step 1 for real instead of trusting the ad hoc diff above: added a permanent test,
`raw_rows_cover_every_introspection_name_in_order` (`src/builtins/native_method_row.rs`), that for
every owner in `BUILTIN_METHOD_OWNERS` (a) asserts every introspection-array name has a matching
`RAW_ROWS` row under the *folded* owner (`canonical_builtin_owner`) and (b) asserts `RAW_ROWS`'s
relative order of the names the two sources share matches the introspection array's order.

**Finding: nothing was actually missing.** The "`Sub` has ZERO rows in `RAW_ROWS`" claim above came
from filtering `RAW_ROWS` by the literal string `"Sub"` and comparing against
`builtin_method_entries("Sub")` — but `builtin_method_entries` already returns `owner: "Code"` (the
folded owner) for every `Sub`/`Method`/`Block`/`Routine`/`Code` entry, and `RAW_ROWS` already stores
all 10 `CODE_METHODS` rows under the key `"Code"` (`name`, `signature`, `arity`, `count`, `of`,
`returns`, `Bool`, `Str`, `gist`, `raku`) in the right order. Filtering the raw table by the
*unfolded* `"Sub"` string and diffing that against the *folded* `builtin_method_entries("Sub")`
output was comparing rows keyed two different ways — a bug in the ad hoc probe script, not a real
gap. Re-running the diff with folding applied on both sides (the permanent test above) found
**zero missing names for all 18 owners**, not just `Sub`.

**Order did diverge for real, for two owners**: `Signature` (its `gist`/`raku` rows were interleaved
into an unrelated hand-added block far from its other six rows) and `Any` (`so`/`not`/`defined`,
`WHERE`, and `gist`/`raku` were similarly scattered into unrelated E2b hand-added blocks). Both
fixed by moving the misplaced rows to the position their introspection array implies
(`native_method_row_table.rs`); no row content changed, order only. The new test now passes for all
18 owners and is a permanent regression guard (previously only 5 owners had even a length check,
`native_method_rows_matches_builtin_entry_count`).

**What step 1 does NOT resolve**: the ~90+ extra `RAW_ROWS` names per owner beyond what the
introspection arrays list (step 2's job) are untouched by this pass — those still need per-name raku
verification before F3 can decide "genuine `.^methods` gap" vs. "deliberately internal/protocol,
dispatch-only" for each one. Step 1 only closes the "is `RAW_ROWS` even a safe superset, in the right
order" question, and the answer for the *introspection-array* side is now yes, permanently enforced.
Step 3 (the actual cutover) still needs step 2 first.

## Progress (2026-08-15): step 2 started, one name (`Mu`'s `DEFINITE`)

Triaged the smallest owner first (`Mu` had exactly one extra name per the survey above). Raku
ground truth: `Mu.^methods` lists `DEFINITE` in real Rakudo, and mutsu already dispatches
`.DEFINITE` correctly (`RAW_ROWS` picked it up via an earlier E2b slice) — introspection was simply
missing it, a genuine gap, not one of the deliberately-internal/protocol names this step also needs
to identify. Added `"DEFINITE"` to `MU_METHODS` (`builtin_type_methods.rs`) at the position
matching its `RAW_ROWS`-relative order (ahead of `defined`), keeping
`raw_rows_cover_every_introspection_name_in_order` green, and pinned with a `works-and-can`/
`.^methods` pair in `t/can-methods-drift.t` (verified against real `raku` output too).

Remaining for step 2: ~89+ names across the other 17 owners (`Any` has 7, `Str` 25, `Int` 25,
`Cool` 11, `Hash` 11, plus smaller counts elsewhere per the survey table above), each needing the
same raku-verify-then-classify treatment (genuine `.^methods` gap vs. deliberately-internal
dispatch-only name). Suggest continuing owner-by-owner in ascending extra-name-count order (small,
independently-landable slices, same pattern as this one), rather than a single large sweep.

## Progress (2026-08-15, continued): `Any` (7 extras) and `Hash` (11 extras) triaged

`Any`: `serial` and `hash` confirmed as genuine `.^methods` gaps (`raku -e 'say
Any.^methods.grep(*.name eq "serial").elems'` → 1, same for `hash`; both already dispatch
correctly on mutsu, e.g. `(1,2,3).serial`, `(a=>1,b=>2).hash`). `self`, `clone`, `WHICH`, `sink`,
`item` confirmed dispatch-only/internal — raku's `Any.^methods` does not list any of them (`elems`
0 for each); these stay unlisted in `ANY_METHODS` by design, not oversight. Added `serial`/`hash`
to `ANY_METHODS`, ahead of `say` (their `RAW_ROWS`-relative position).

`Hash`: of the 11 extras (`pick`, `EXISTS-KEY`, `AT-KEY`, `List`, `invert`, `flat`, `Array`,
`AT-POS`, `EXISTS-POS`, `dynamic`, `roll`, `perl` — 12 listed in the original survey, one,
`perl`, was miscounted as one of the "11"), 8 are genuine gaps (`pick`, `EXISTS-KEY`, `AT-KEY`,
`List`, `invert`, `flat`, `dynamic`, `roll` — each confirmed present on real `Hash.^methods` and
already dispatching correctly, e.g. `%h.pick`, `%h.EXISTS-KEY('a')`, `%h.List`, `%h.invert`).
`Array`, `AT-POS`, `EXISTS-POS`, `perl` confirmed dispatch-only (not on real `Hash.^methods`).
Added the 8 genuine names to `HASH_METHODS`, appended after the array's existing tail (their
`RAW_ROWS` rows arrive in a second block, after `Int`) to keep
`raw_rows_cover_every_introspection_name_in_order` green. All raku-verified and pinned in
`t/can-methods-drift.t`.

Running total: 3 of 18 owners fully triaged (`Mu`, `Any`, `Hash`). Largest remaining: `Str` (25
extras), `Int`/`Num`/`Rat`/`Complex` (25, likely shared via `NUMERIC_OWN`), `Cool` (11).

## Progress (2026-08-15, continued): `Cool` (11 extras) triaged, plus a real bug found

`Cool`'s 11 extras are the native-sized-integer coercion methods (`int8`, `int16`, `int32`,
`int64`, `uint8`, `uint16`, `uint32`, `uint64`, `byte`, `int`, `uint`). All 11 raku-verified as
genuine `Cool.^methods` entries (`raku -e 'say Cool.^methods.grep(*.name eq "int8").elems'` → 1,
same for the rest) and already dispatch correctly on mutsu (`300.int8` → 44, etc.) — the
`native_method_row_table.rs` comment above these rows previously claimed they were "deliberately
excluded from `.^methods`/`.^can`-by-list", but that conflated two different concerns: the actual
exclusion (`NATIVE_INT_TYPES` vs. `NATIVE_INT_COERCE_METHODS` in `runtime/native_types.rs`) is
about *type-alias* names (`bool`, `long`, `ulong`, ...) that name a type but are not methods, which
is unrelated to whether the 11 real coercion methods belong in `COOL_OWN`. Corrected that comment
in place. Added a new `COOL_NATIVE_INT_COERCE_TAIL` array, appended after `NUMERIC_COERCIONS` in
the `"Cool"` match arm (matching the block's position in `RAW_ROWS`, required to keep
`raw_rows_cover_every_introspection_name_in_order` green).

**This surfaced a real, previously-latent bug**: `is_builtin_type_method`
(`methods_classhow_lookup.rs`), which backs `.^find_method`/`.can` on a `Package` receiver via
`classhow_find_method`, unconditionally checked `["type_name", "Cool", "Any", "Mu"]` as the
ancestor list for every type, regardless of whether `Cool` was genuinely an ancestor. This was
harmless while `Cool`'s own introspection list had no method name likely to collide with a
non-Cool type's probe, but the moment `int8` etc. joined `Cool`'s list, `Pair.^can('int8')`
(`Pair`'s real MRO is `[Pair, Any, Mu]` per `builtin_type_catalog.rs` — no `Cool`) flipped from
correctly `False` to a false-positive `True`, caught immediately by the existing
`t/native-int-coerce-methods-are-cool-only.t` pin ("Pair is not Cool, so it cannot int8"). Fixed by
reading the receiver type's real MRO via `registry().class_mro_readonly()` (the same authoritative
builtin-type-catalog source `classhow_lookup_impl` already uses a few lines above) instead of the
hardcoded guess, falling back to the old `[type_name, "Cool", "Any", "Mu"]` heuristic only when the
catalog doesn't recognize the type name at all. Added a matching regression pin to
`t/can-methods-drift.t`. Full local `t/` suite (3166 files, all green) plus the targeted
`S12-introspection/*`, `S02-types/hash.t`, `S09-typed-arrays/hashes.t` roast files confirm no other
regression.

Running total: 4 of 18 owners fully triaged (`Mu`, `Any`, `Hash`, `Cool`). Largest remaining: `Str`
(25 extras), `Int`/`Num`/`Rat`/`Complex` (25, likely shared via `NUMERIC_OWN`).

## Progress (2026-08-15, continued): `Int`/`Num`/`Rat`/`Complex` triaged -- the "shared" guess was wrong

The earlier note above guessed `Int`/`Num`/`Rat`/`Complex` share the same 25 extras via the common
`NUMERIC_OWN` array. Checked `RAW_ROWS` directly instead of assuming: only `Int` actually has a
25-name extras block; `Num` has zero; `Rat` has 2 (`FatRat`, `nude`); `Complex` has 8
(`UInt`, `isNaN`, `re`, `im`, `reals`, `conj`, `reverse`, `Complex`).

`Int`: 7 of 25 genuine (`rand`, `uniprop`, `lsb`, `msb`, `int8`, `Real`, `Complex` -- all
raku-verified present on `Int.^methods` and already dispatching, e.g. `5.rand`, `65.uniprop`,
`5.lsb`, `5.msb`, `5.int8`, `5.Real`, `5.Complex`). The other 18 confirmed dispatch-only.

`Rat`: both extras genuine (`(1/3).FatRat`, `(1/3).nude` both raku-verified and dispatch
correctly).

`Complex`: 6 of 8 genuine (`isNaN`, `re`, `im`, `reals`, `conj`, `Complex` -- raku-verified and
dispatching, e.g. `(1+2i).isNaN`, `.re`, `.im`, `.reals`, `.conj`, `.Complex`). `UInt`/`reverse`
confirmed dispatch-only (real `Complex.^methods` doesn't list either).

Since `NUMERIC_OWN` is one array shared by all four owners but these extras are genuinely
per-owner (not shared) -- `RAW_ROWS` itself only lists `rand` under `"Int"`, even though real
Rakudo's `Num`/`Rat` also have a working `.rand` (a separate, still-open gap outside `RAW_ROWS`'s
own claims, so outside F3's "match `RAW_ROWS`" scope) -- split the combined
`"Int" | "Num" | "Rat" | "Complex"` match arm into four, each with its own optional extra tail
(`INT_EXTRA_TAIL`, `RAT_EXTRA_TAIL`, `COMPLEX_EXTRA_TAIL`) appended after `NUMERIC_COERCIONS`,
positioned to match each block's location in `RAW_ROWS`. All raku-verified and pinned in
`t/can-methods-drift.t` (96 assertions total). Full local `t/` suite (3167 files) and the targeted
`S12-introspection/*` plus every `S32-num/*.t` roast file stay green.

Running total: 8 of 18 owners now settled (`Mu`, `Any`, `Hash`, `Cool`, `Int`, `Num`, `Rat`,
`Complex` -- `Num` needed no changes, its `RAW_ROWS` extras block was empty). `Str` (25 extras) is
now the only large owner left untriaged; the rest are the smaller-count owners from the original
survey.

## Progress (2026-08-15, continued): `Str` (24 extras, the survey's "25" was off by one) triaged

11 of 24 genuine `Str.^methods` gaps: `uniprop`, `indent`, `ord`, `uniname`, `uninames`, `unival`,
`univals`, `tclc`, `Version`, `Date`, `DateTime` -- all raku-verified present on `Str.^methods` and
already dispatching correctly (`'A'.ord` -> 65, `65.uniname` -> "LATIN CAPITAL LETTER A",
`'1.2.3'.Version` -> v1.2.3, etc.). The other 13 (`AST`, `list`, `UInt`, `FatRat`, `sprintf`,
`chrs`, `bytes`, `Range`, `Complex`, `Real`, `reverse`, `byte`, `perl`) confirmed dispatch-only --
real Rakudo's `Str.^methods` lists none of them.

Added a new `STR_EXTRA_TAIL`, appended after the `&["elems", "fmt"]` tail already in the `"Str"`
match arm, positioned to match the extras block's location in `RAW_ROWS`. All raku-verified and
pinned in `t/can-methods-drift.t` (129 assertions total). Full local `t/` suite (3167 files) green.

**Invocation gotcha hit while verifying roast, not a regression**: a bare `prove -e
'target/debug/mutsu' roast/S32-str/*.t` spuriously fails 3 files
(`gb18030-encode-decode.t`/`gb2312-encode-decode.t`/`shiftjis-encode-decode.t`) with "No such file
or directory" on their fixture paths -- those tests resolve `t/spec/...` relative paths that only
exist when run through `scripts/run-roast-test.sh` (per `MUTSU_BIN=... prove -e
'scripts/run-roast-test.sh' roast/<path>.t` from CLAUDE.md), which the direct-binary invocation
skips. Re-ran the correct way and all of `roast/S32-str/*.t` passes clean; unrelated to this
change.

**This closes F3 step 2's large-owner sweep**: `Str`, `Int`, `Cool`, `Complex`, and `Any` -- every
owner the original survey flagged with 7+ extras -- are now triaged (9 of 18 owners settled
counting `Mu`/`Hash`/`Rat`/`Num` too). The remaining ~9 owners left for step 2 each have only 1-3
extras per the original survey table (much smaller individual slices); step 3 (the actual
`RAW_ROWS`-as-single-source cutover) can reasonably start once those are swept.

## Progress (2026-08-15, continued): `List`/`Array`/`Range`/`Blob` triaged -- the "1-3 extras" estimate was also wrong, and step 2 is now COMPLETE

Rather than trust the original survey's rough per-owner extras counts for the remaining 9 owners,
ran a fresh throwaway probe (temporary `#[cfg(test)]` in `native_method_row.rs`, not committed --
iterate `BUILTIN_METHOD_OWNERS`, diff each owner's `RAW_ROWS` names against its current
introspection array, print non-empty diffs) to get exact current numbers. Result: `List` has 18
extras, `Array` 19, `Range` 13, `Blob` 7 -- all far larger than "1-3". `Bool`, `Sub`, `Signature`,
`IO::Path`, `IO::Handle` printed nothing at all: genuinely **zero** extras, already fully covered.

`List`: 13 of 18 genuine (`list`, `item`, `Slip`, `sink`, `invert`, `AT-POS`, `EXISTS-POS`,
`is-lazy`, `Capture`, `hyper`, `race`, `Supply`, `fmt`), raku-verified and already dispatching
(`(1,2,3).list`, `.item`, `.Slip`, `.sink`, pair-list `.invert`, `.AT-POS(0)`, `.EXISTS-POS(0)`,
`.is-lazy`, `.Capture`, `.hyper`, `.race`, `.Supply`, `.fmt('%d')`). `cache`/`WHICH`/`tree`/
`pairup`/`hash` confirmed dispatch-only for `List` specifically -- real `List.^methods` omits them.

`Array`: same 13 as `List` PLUS 2 more real Rakudo answers only for `Array`: `WHICH` and
`dynamic` -- confirmed against raku directly (`Array.^methods` includes `WHICH`, `List.^methods`
doesn't), not assumed from `RAW_ROWS` alone. Since `LIST_METHODS` is one array shared by both
`"List"` and `"Array"` match arms but their genuine extras differ, split into
`LIST_EXTRA_TAIL`/`ARRAY_EXTRA_TAIL` (same shared-base-plus-per-owner-tail pattern as the
`Int`/`Rat`/`Complex` split earlier in this file).

`Range`: 7 of 13 genuine (`hyper`, `lazy`, `int-bounds`, `AT-POS`, `race`, `in-range`,
`EXISTS-POS`), raku-verified and dispatching (`(1..5).hyper`, `.lazy`, `.int-bounds`, `.AT-POS(0)`,
`.race`, `.in-range(3)`, `.EXISTS-POS(0)`). `Array`/`join`/`Supply`/`List`/`head`/`batch`
confirmed dispatch-only for `Range`.

`Blob`/`Buf`: 5 of 7 genuine (`read-uint8`, `read-int8`, `read-uint16`, `read-int16`,
`read-uint32`), raku-verified and dispatching (`Buf.new(...).read-uint8(0)` etc.). `values`/`List`
confirmed dispatch-only for `Blob`.

All 25 additions pinned in `t/can-methods-drift.t` (now 193 assertions). Full local `t/` suite
(3167 files) green; `roast/S12-introspection/*`, `S02-types/{array,list,range}.t`,
`S32-container/buf.t`, `S03-operators/buf.t`, and every `S03-buf/*.t` file green (via
`scripts/run-roast-test.sh`).

**F3 step 2 is now complete**: 13 of 18 owners needed real additions (`Mu`, `Any`, `Hash`, `Cool`,
`Int`, `Rat`, `Complex`, `Str`, `List`, `Array`, `Range`, `Blob` -- 12 -- plus `Num` confirmed
zero-change). The other 5 (`Sub`, `Signature`, `IO::Path`, `IO::Handle`, `Bool`) were confirmed to
already have zero `RAW_ROWS`/introspection drift. Every one of the 18 `BUILTIN_METHOD_OWNERS` has
now been raku-verified against its `RAW_ROWS` extras. **Step 3 (the actual "delete the 14 arrays,
read `builtin_method_entries`/`builtin_type_method_names` from `RAW_ROWS` directly" cutover) is
now unblocked** -- every genuine gap this step's classification needed has been closed, and every
dispatch-only name has been confirmed and left out on purpose. Step 3 itself still needs its own
design pass (how to encode "dispatch-only, not introspectable" as a flag on `NativeMethodRow`
rather than by omission from a second array, per this file's original "suggested next step"
section) -- not started here.
