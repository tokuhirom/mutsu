# ADR-0019 F3 step 2: `Mu`, `Any`, `Hash`, `Cool` introspection gaps (plus a real `.^can` bug)

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

Then triaged `Cool`'s 11 extras — the native-sized-integer coercion methods (`int8`..`uint64`,
`byte`, `int`, `uint`). All 11 raku-verified as genuine `Cool.^methods` entries that already
dispatch correctly on mutsu. Adding them surfaced a real bug, not just a list gap:
`is_builtin_type_method` (backing `.^find_method`/`.can` on a `Package` receiver) unconditionally
checked `["type_name", "Cool", "Any", "Mu"]` as every type's ancestor list, regardless of whether
`Cool` genuinely was one. That was harmless until `Cool`'s own list grew to include a name likely
to collide — once `int8` joined it, `Pair.^can('int8')` (`Pair`'s real MRO has no `Cool`) flipped
from correctly `False` to a false-positive `True`, caught immediately by the pre-existing
`t/native-int-coerce-methods-are-cool-only.t` pin. Fixed by reading the receiver type's real MRO
from the builtin type catalog instead of guessing, with a new regression pin added alongside the
`Cool` names.

4 of 18 catalog owners are now fully triaged (`Mu`, `Any`, `Hash`, `Cool`). Step 2 remains open for
the largest remaining owners, `Str` (25 extras) and `Int`/`Num`/`Rat`/`Complex` (25, likely shared
via `NUMERIC_OWN`); see `todo/deep/adr0019-f3-raw-rows-drift-from-introspection-arrays.md` for the
running list.
