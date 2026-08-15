# ADR-0019 F3 step 2: `Mu`, `Any`, and `Hash` introspection gaps

ADR-0019 Phase F box F3 ("delete the per-type method-name lists, retain only the generated native
entry catalog") found in an earlier scoping pass that its target catalog, `native_method_row.rs`'s
`RAW_ROWS`, carries roughly 90+ names per owner that the 14 hand `builtin_type_methods.rs` arrays
don't list. Each of those names needs a raku ground-truth check before F3 can classify it as a
genuine `.^methods` gap or a deliberately-internal/protocol-only name dispatch recognizes but
introspection correctly omits.

Triaged the first and smallest case: `Mu`'s sole extra name, `DEFINITE`. `raku -e 'say
5.DEFINITE'` works in real Rakudo and `Mu.^methods` lists it there. mutsu already dispatched
`.DEFINITE` correctly (an earlier E2b slice had added it to `RAW_ROWS` for cascade coverage) — only
the introspection-facing `MU_METHODS` array was missing it, so `Mu.^methods`/`.^can('DEFINITE')`
under-reported. Added `DEFINITE` to `MU_METHODS` at the position matching its `RAW_ROWS`-relative
order (ahead of `defined`), which keeps the `raw_rows_cover_every_introspection_name_in_order`
regression guard green, and pinned it in `t/can-methods-drift.t` (verified against real `raku`
output).

Continued the same triage for `Any` (7 extras) and `Hash` (11 extras). `Any`'s `serial` and
`hash` and `Hash`'s `pick`/`EXISTS-KEY`/`AT-KEY`/`List`/`invert`/`flat`/`dynamic`/`roll` were all
confirmed as genuine `.^methods` gaps against real `raku` output and already-working dispatch;
`Any`'s `self`/`clone`/`WHICH`/`sink`/`item` and `Hash`'s `Array`/`AT-POS`/`EXISTS-POS`/`perl`
were confirmed as deliberately dispatch-only names real Rakudo's `.^methods` correctly omits.
Added the genuine names to `ANY_METHODS`/`HASH_METHODS` and pinned all of them in
`t/can-methods-drift.t`.

3 of 18 catalog owners are now fully triaged (`Mu`, `Any`, `Hash`). Step 2 remains open for the
~80+ names across the other 15 owners (`Str` has 25, `Int`/`Num`/`Rat`/`Complex` 25, `Cool` 11
being the largest); see `todo/deep/adr0019-f3-raw-rows-drift-from-introspection-arrays.md` for the
running list and suggested owner-by-owner ordering.
