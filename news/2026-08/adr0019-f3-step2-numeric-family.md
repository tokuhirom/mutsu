# ADR-0019 F3 step 2: `Int`/`Rat`/`Complex` introspection gaps

Continued the ADR-0019 Phase F box F3 raku-verification triage of names `native_method_row.rs`'s
`RAW_ROWS` catalog recognizes for dispatch but the per-type `.^methods` arrays in
`builtin_type_methods.rs` don't list.

The numeric family (`Int`/`Num`/`Rat`/`Complex`) was previously assumed to share one 25-name extras
set via the common `NUMERIC_OWN` array. Checking `RAW_ROWS` directly instead of assuming showed
that guess was wrong: only `Int` actually carries a 25-name extras block; `Num` has none; `Rat` has
2 (`FatRat`, `nude`); `Complex` has 8.

Raku-verified each: `Int` gained `rand`, `uniprop`, `lsb`, `msb`, `int8`, `Real`, and `Complex` (7
of its 25 extras — the other 18 confirmed dispatch-only, real Rakudo's `Int.^methods` doesn't list
them). `Rat` gained both `FatRat` and `nude`. `Complex` gained `isNaN`, `re`, `im`, `reals`,
`conj`, and `Complex` (6 of 8 — `UInt`/`reverse` confirmed dispatch-only). All 15 additions
already dispatched correctly before this change; only `.^methods` enumeration was missing them.

Since these extras are genuinely per-owner rather than shared, the combined
`"Int" | "Num" | "Rat" | "Complex"` match arm in `builtin_type_method_names` was split into four,
each with its own optional extra tail appended after the shared coercion methods. All raku-verified
and pinned in `t/can-methods-drift.t`, which now carries 96 assertions.

8 of 18 catalog owners are now settled (`Mu`, `Any`, `Hash`, `Cool`, `Int`, `Num`, `Rat`, `Complex`
— `Num` needed no changes, its extras block was empty). `Str` (25 extras) is the only large owner
left untriaged; see `todo/deep/adr0019-f3-raw-rows-drift-from-introspection-arrays.md` for the
running list.
