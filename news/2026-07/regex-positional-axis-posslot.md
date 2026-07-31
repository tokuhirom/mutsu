# Regex positional capture axis collapsed into `Vec<PosSlot>` (ADR-0016 P4, layer 1)

The regex engine's positional capture state was five parallel collections —
`positional` (captured text) ‖ `positional_subcaps` ‖ `positional_quantified` ‖
`positional_offsets` ‖ `positional_nil` — kept index-aligned by padding
invariants asserted only in comments, on both the mutable accumulator
(`RegexCaptures`) and the stored capture node (`CapChildren`). PR #5592
collapses them into a single axis, `Vec<PosSlot { from, to, subcap,
quantified, nil }>`.

The load-bearing change is that the span now survives onto stored nodes:
`into_cap_node` used to drop `positional_offsets`, so a subcap-less stored
leaf had no offsets anywhere and the Match builder fabricated `0..len` from
the captured text. With the slot carrying its span, that text-leaf fallback is
retired for matcher output — positional capture leaves report their real
`.from`/`.to` — and the stored text axis is deleted outright; every reader
derives text from the recorded span through the shared `MatchTarget` (or the
engine's `chars`). Backreferences (`$0` inside a pattern) became alloc-free
span comparisons against the same `chars` the match runs in.

The trail's undo vocabulary shrank with the axis: `PosLens` recorded five
lengths, now one; the four-vector `PosTailRec` is one slot vector; the
`truncate_positional_3`/`_4` asymmetry and the `positional_nil` padding loop
are gone because the invariants are structural now. `fold_quantified_captures`
and `reserve_nil_capture_slots` rewrote to a fraction of their size, and the
fold's `(0, len)` span-fabrication fallback disappeared.

Deliberate non-changes: `positional_slots` (the pcre2/`:P5` numbering axis,
which has `None` holes) stays separate; `CodeBlockContext.positional` keeps
its text-snapshot shape but is materialized from spans at its single
construction site in the same engine space; text-only carriers
(transliteration callbacks, vm-subst `$N` vectors, `SplitMatch`) keep their
`Vec<String>` shape via `make_match_object_with_captures`.

Corrective behavior changes (toward raku, consistent with P3a's precedent for
the subcap axis): real leaf offsets as above, and under `:m`/`:i` multi-char
folds the capture texts read through consumers (subst `$N` interpolation,
find-first capture lists) now derive from the original subject instead of the
mark-stripped/case-folded engine space.

Net −265 lines. Verified: `cargo test` 628, `t/` 24923, and a full local
`make roast` (1435 files, 218774 tests) all pass.
