# A successful `~~` skips the cycle-candidate buffer and two redundant per-match steps

Three costs a regex match paid that it did not need:

- **Cycle-candidate buffering of an unread Match.** A `~~` hit hands its
  `Match` to `$/` and to the expression's result. Dropping the result left the
  node alive, so the cycle collector buffered it as a possible cycle root on
  every hit. That push cost ~160 instructions, and the `Weak` it pushed kept the
  allocation until the next drain. A lazy Match has no `Gc` edge until it is
  materialized, so `MatchNode` now answers `currently_acyclic` until then.
- **The reduce walk with nothing to reduce.** `reduce_regex_captures_made_for_rule`
  cloned the node's `regex_vars` and called the dynvar install, child walk
  and restore. Each of those returns at once when no grammar declares a
  `:my $*x`. It now checks that first.
- **A second `regex_value_facts` lookup.** `regex_match_with_captures_value`
  looked the facts up, then `parse_regex_value` looked them up again. That is a
  thread-local probe and a compare of the pattern text, on every attempt. The
  facts now pass through (`parse_regex_value_with_facts`).

On 6,000 `$l ~~ / ^ '2026' /` hits on a 45-char line, a hit costs ~590
instructions less and a miss ~180 less.

`scripts/bench-det.sh`, release build, `main` `e5848074` vs this change:

| benchmark | instructions |
|---|---|
| bench-regex-match | 264.3M → 259.3M (-1.9%) |
| bench-regex-assertion | 161.6M → 158.5M (-1.9%) |
| bench-regex-capture | 346.0M → 343.0M (-0.9%) |
