# JSON::Fast encode and decode are benchmarked separately

`bench-json-fast` times seven `to-json` and four `from-json` calls of a META6-shaped
document, plus a long-string round trip, as one section. On 2026-11-28 its `@section+jit`
row read 21.7x rakudo, and splitting the section by phase showed the two halves are
unrelated: on a release build the META encode took ~4.4 ms per call and the decode ~0.4 ms,
against rakudo's ~0.5-1.3 ms and ~1-2.5 ms. One row could not say which half moved.

`bench-json-fast-encode` (60 `to-json` calls) and `bench-json-fast-decode` (300 `from-json`
calls) now time each phase alone on the same document, each tracked against #8673.
`bench-json-fast` stays as the combined trend line and the long-string complexity guard.
