# Typed (wide-element) Buf native-interop holes

Side findings from the DBIish mysql parity campaign (2026-07-29, the session
that fixed the `& 0xff` element-assign truncation and added the CArray
element-write arm). None block the DBIish suites, but each was a real
divergence from Rakudo when found.

Items 2-4 were re-verified 2026-08-06 (S-effort sweep off `todo/TRIAGE.md`):
item 2 no longer reproduces (fixed by other work since 2026-07-29 — see
`t/buf-wide-read-write-int.t`, already whitelisted); items 3 and 4 did
reproduce and are now fixed — see
`news/2026-08/buf-nested-index-assign-and-parametric-native-param.md`.

Only item 1 remains, and it does **not** currently reproduce either: the
straightforward repro (`constant intptr = uint64; Buf[intptr].new(...)`)
resolves the alias correctly today, matching raku (`Buf[uint64]`, width 8).
Left open only because the underlying fragility is still real, not because
anything concretely fails right now.

1. **`Buf[intptr]`-style alias fallback picks width 1 -- IF alias resolution
   ever fails, not reproduced today.** `BufData` width is derived once from
   the class-name string (`buf_elem_width`, `src/value/value_buf.rs`):
   substring probe for "64"/"32"/"16", else 1. Parameterization normally
   resolves `constant intptr = uint64` to the class name `Buf[uint64]` before
   `buf_elem_width` ever sees it, but if some *other* alias path ever handed
   the unresolved name through instead, `"Buf[intptr]"` would silently probe
   to width 1 + signed -- a differently-shaped buffer with no error. The
   probe should fail loudly (or resolve through the registry) instead of
   guessing, but finding a concrete unresolved-alias repro is a prerequisite
   for justifying the change (see "don't add error handling for scenarios
   that can't happen", CLAUDE.md).

Minimal repros are one-liners; see `news/2026-07/dbiish-upstream-suite-parity.md`
for the original campaign context.
