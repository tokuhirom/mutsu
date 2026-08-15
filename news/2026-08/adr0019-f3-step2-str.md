# ADR-0019 F3 step 2: `Str.^methods` introspection gaps close the large-owner sweep

Continued the ADR-0019 Phase F box F3 raku-verification triage of names `RAW_ROWS` recognizes for
dispatch but the per-type `.^methods` introspection arrays don't list. `Str` carried a 24-name
extras block (the original survey's "25" count was off by one).

Raku-verified all 24: `uniprop`, `indent`, `ord`, `uniname`, `uninames`, `unival`, `univals`,
`tclc`, `Version`, `Date`, and `DateTime` are genuine `Str.^methods` gaps — all already dispatch
correctly on mutsu (`'A'.ord` → 65, `65.uniname` → "LATIN CAPITAL LETTER A", `'1.2.3'.Version` →
v1.2.3, etc.). The other 13 (`AST`, `list`, `UInt`, `FatRat`, `sprintf`, `chrs`, `bytes`, `Range`,
`Complex`, `Real`, `reverse`, `byte`, `perl`) are confirmed dispatch-only — real Rakudo's
`Str.^methods` lists none of them. Added a new `STR_EXTRA_TAIL` array and pinned all 11 additions
in `t/can-methods-drift.t`, which now carries 129 assertions.

This closes F3 step 2's sweep of every large owner the original survey flagged with 7+ extras
(`Str`, `Int`, `Cool`, `Complex`, `Any`) — 9 of 18 catalog owners are now fully settled. The
remaining ~9 owners each have only 1-3 extras per the original survey, much smaller individual
slices; see `todo/deep/adr0019-f3-raw-rows-drift-from-introspection-arrays.md` for the running
list.
