# `nqp::ordat`/`nqp::eqat` no longer turn a big JSON parse into a hang

`Test::META`'s own test suite (`ecosystem/dists/T/Test--META~6791d22b.json`) was `partial`:
`t/020-internals.t` hit the ecosystem sweep's 240s timeout with zero assertions run, and
`t/030-my-meta.t` failed its one subtest. Both exercise `check-license`, which constructs a
`License::SPDX` object — and `License::SPDX.new` deserializes a bundled 332KB JSON file (the full
SPDX license list) via `JSON::Class`/`JSON::Unmarshal`, which in turn parses it with `JSON::Fast`.

`JSON::Fast` is "a naive imperative JSON parser in pure Raku... with direct access to `nqp::` ops"
for speed — real MoarVM gives every one of those ops O(1) codepoint indexing. Two of them didn't in
mutsu:

- `nqp::ordat($str, $pos)` cloned its whole string argument via `to_string_value()` (an
  allocating copy) and then walked it with `s.chars().nth(p)` — another full linear scan — **on
  every single call**.
- `nqp::eqat($haystack, $needle, $pos)` did the same clone-then-collect over its haystack.

JSON::Fast's `nom-ws` whitespace-skipping loop calls `nqp::ordat` once per character while scanning
past every character of the document, and its string-token fast path calls `nqp::eqat` against the
same full text. Over a document of length *n* that is O(n) work repeated O(n) times: a synthetic
30KB SPDX-shaped JSON file already took 14+ seconds to parse, and the real 332KB list timed out
entirely.

The fix reuses `nqp_char_cache` — a single-slot `Value::Str -> Rc<Vec<char>>` memo already applied to
`substr`/`index`/`rindex`/`indexic*`/`iscclass`/`findcclass`/`findnotcclass` for this exact access
pattern (a hand-rolled NQP scanner calling a text op repeatedly against the SAME string with an
advancing position). `ordat` and `eqat` now go through it too, making each call O(1) amortized
instead of O(n).

## Result

`Test::META` 0.0.20 goes `partial` (1/3 parity files) → **`green`** (3/3 files, 26/26 assertions,
exact parity with rakudo). `t/020-internals.t` finishes in ~32s on a release build (was a 240s
timeout); `t/030-my-meta.t` passes its `meta-ok()` subtest in full.

Pinned by extending `t/vm/nqp-text-scan-cache.t` — the existing home for this class of fix — with
`ordat` coverage over a 20,000-character string scanned position-by-position, the same shape as
JSON::Fast's own loop.
