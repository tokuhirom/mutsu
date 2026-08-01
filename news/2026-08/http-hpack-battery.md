# HTTP::HPACK bundled as the HTTP/2 header-compression battery

`HTTP::HPACK` (`zef:raku-community-modules`, v1.0.3, Artistic-2.0) is
vendored at `modules/HTTP-HPACK/` and resolves with zero config. Both
upstream test files — 57 subtests covering the full RFC 7541 appendix-C
example suite, Huffman coding, and dynamic-table eviction — pass, matching
raku. It is a hard dependency of Cro::HTTP (HTTP/2 support) — the fourth
Cro::HTTP dependency locked in behind the release gate.

One general interpreter fix got it there; the vendored source is untouched:

- **`xx` thunks its left side** — the lhs expression is re-evaluated for
  every repetition (`rand xx 10` is ten different numbers), where mutsu
  evaluated once and repeated the value unless the lhs matched a whitelist
  of "known side-effecting calls" (`rand`, `.push`, …) — exactly the
  incomplete static analysis CLAUDE.md warns about. HPACK's
  `decode-str($packed, $idx) xx 2` (read a header's name, then its value,
  advancing the rw offset through two calls) is a plain user sub call no
  whitelist can enumerate. The predicate is now inverted: only provably
  pure value expressions (literals, plain variable reads, and composites
  of those) repeat their value; everything else re-evaluates. A small
  literal count unrolls inline in the current frame, which also sidesteps
  a pre-existing closure rw-writeback gap (an `is rw` argument's writeback
  through a closure loses updates after the first call — filed as
  `todo/tickets/closure-rw-arg-writeback.md`). Pin: `t/xx-thunk-reeval.t`.

Packaging: `batteries.lock` row + both files whitelisted in the release
gate, `t/http-hpack-battery.t` smoke test (RFC examples, static-table
decode), the selection record `docs/batteries/http-hpack.md`, the
BATTERIES.md §7 index row, and a site row via
`scripts/gen-batteries-manifest.py`.
