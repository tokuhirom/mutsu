# `<sym>` no longer costs a proto its subrule-resolution cache

A `token value:sym<true> { <sym> }` variant — the common shape for a
proto-dispatch grammar (`JSON::Tiny::Grammar` among them) — lowers `<sym>` to
a named-capture binding, `:ratchet $<sym>=[true]`. `regex_pattern_is_static`
misread that `$<...>` as runtime variable interpolation, and
`resolve_parsed_token_candidates_in_pkg` declined its whole-proto memo the
moment any one candidate looked non-static — so a proto with three
`:sym<...>` variants using `<sym>` re-ran the full registry walk (which
formats every candidate's pattern text from scratch) on **every single**
`<value>` reference in the document, not just the ones that actually needed
it.

Two fixes:

- `$<name>` (a named backreference) and `$<name>=`/`@<name>=`/`%<name>=`
  (named-capture bindings) are resolved from their literal name text alone —
  `interpolate_regex_scalars`, the actual substitution pass the predicate
  exists to gate, has no `$</@</%<` branch at all. `regex_pattern_is_static`
  no longer treats `<` as starting a variable form.
- Even with that fixed, a proto can still have a genuinely dynamic sibling
  candidate. `resolve_parsed_token_candidates_in_pkg` now caches the raw
  (pre-parse) candidate list — the expensive part — regardless of
  staticness, and only skips the persistent fully-parsed cache when some
  candidate really is dynamic. That one candidate now re-pays just the cheap
  per-call `parse_regex` step, never the registry walk, on every reference.

Measured (steady-state, release, `benchmarks/bench-grammar-parse-big.raku`'s
own grammar): mutsu goes from ~1.8x slower than warm rakudo to ~1.8x faster on
this shape.

A new `MUTSU_VM_STATS` counter (`regex-raw-token-candidates-cache`) pins the
second fix deterministically — a timing assertion would be flaky — and Rust
unit tests pin the classifier fix directly.

See [#8265](https://github.com/tokuhirom/mutsu/issues/8265).
