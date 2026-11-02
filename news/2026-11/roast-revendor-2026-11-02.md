# Re-vendored roast at fb1d8721 (2026-11-01)

`roast/` moved from `1f749e33` (2026-09-29) to `fb1d8721` (2026-11-01). Most of the churn is the
Unicode 17 -> 18 regeneration of the normalization, grapheme-break, collation and emoji tests,
plus removed `#?rakudo.js` fudge lines and new regression tests (ratchet + capture, `Str()`
coercion in a user `infix:<~>`, `AT-POS` on a scalar, `uint8` attribute `clone`, `CALL-ME` on a
`Callable` constant, UTF8-C8 grapheme assembly). `.sort` now expects a `List` rather than a `Seq`.

Five whitelisted files no longer pass completely and were moved to `TODO_roast/BLOCKERS.md`:
`GraphemeBreakTest-0.t`, `GraphemeBreakTest-3.t`, `utf8-c8.t` (#15931),
`S06-operator-overloading/infix.t` (#15932) and `S09-multidim/XX-POS-on-undimensioned.t` (#15933).
