# ADR-0019 F3 step 2 complete: every catalog owner's introspection gap triaged

Finished the ADR-0019 Phase F box F3 raku-verification triage that started earlier this week: for
every one of the 18 `BUILTIN_METHOD_OWNERS`, checking each name `native_method_row.rs`'s `RAW_ROWS`
catalog recognizes for dispatch but the per-type `.^methods` introspection arrays in
`builtin_type_methods.rs` don't list, and classifying it as a genuine `.^methods` gap (add it,
raku-verified) or deliberately dispatch-only (leave it out, real Rakudo's own `.^methods` omits it
too).

The original survey's rough extras counts for the remaining owners ("1-3 extras each") turned out
to be as inaccurate as the earlier "shared 25 extras" guess for the numeric family: a fresh direct
diff found `List` actually carries 18 extras, `Array` 19, `Range` 13, and `Blob` 7. `List` gained
13 genuine names (`list`, `item`, `Slip`, `sink`, `invert`, `AT-POS`, `EXISTS-POS`, `is-lazy`,
`Capture`, `hyper`, `race`, `Supply`, `fmt`); `Array` gained those same 13 plus two more real
Rakudo answers only for `Array` specifically (`WHICH`, `dynamic`) — confirmed directly against
raku, not assumed. `Range` gained 7 (`hyper`, `lazy`, `int-bounds`, `AT-POS`, `race`, `in-range`,
`EXISTS-POS`). `Blob`/`Buf` gained 5 (`read-uint8`, `read-int8`, `read-uint16`, `read-int16`,
`read-uint32`). All 25 already dispatched correctly before this change. Since `List`/`Array` share
one introspection array (`LIST_METHODS`) but need different extras, split their lookup into
separate `LIST_EXTRA_TAIL`/`ARRAY_EXTRA_TAIL` tails, mirroring the split already used for
`Int`/`Rat`/`Complex`.

The remaining 5 owners (`Sub`, `Signature`, `IO::Path`, `IO::Handle`, `Bool`) turned out to already
have zero drift — nothing to add. All additions across this whole triage effort are pinned in
`t/can-methods-drift.t`, which now carries 193 assertions, each verified against real `raku`
output.

**Every one of the 18 catalog owners has now been triaged.** Step 3 — the actual cutover that lets
`builtin_method_entries`/`builtin_type_method_names` read from `RAW_ROWS` directly and retires the
14 hand-written arrays — is unblocked, though it still needs its own design pass for how to encode
"dispatch-only, not introspectable" as data (a flag on `NativeMethodRow`) rather than by omission
from a second array. See `todo/deep/adr0019-f3-raw-rows-drift-from-introspection-arrays.md` for the
full history and the suggested next step.
