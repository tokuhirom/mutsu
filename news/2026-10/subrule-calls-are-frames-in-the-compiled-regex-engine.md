# A `<subrule>` call is a frame in the compiled regex engine (ADR-0135 Slice D, first part)

Until now a pattern that called a grammar rule stayed on the tree walk: `<value>` was the largest
decline reason (`subrule`, 521 patterns across `t/` and the roast whitelist). The compiled engine now
runs a `<subrule>` call as a frame in its own loop, so a grammar parse runs as one compiled program
instead of a tree walk that enters the engine at the leaves.

- A plain rule, or a proto whose candidates all have programs, gets a register window, a capture
  level of its own and an `Rc`-linked frame. Its choice points go on the caller's backtrack stack; its
  return files the callee's captures as the subrule's Match with the walk's own builder. A ratcheted
  call (`token`, `rule`) cuts the callee's choice points when it returns. A call that is not
  ratcheted leaves them, and every choice point remembers its frame, so a failure after the callee
  returned resumes inside it: `regex w { \w+ }` called as `<w> 'd'` still gives back.
- A proto ranks its candidates at the call with the walk's LTM measurement and enters the first that
  matches; the winner's Match carries its `:sym<…>`.
- Everything the frame shape does not cover (arguments, `$*` parameters, wrapped tokens, a custom HOW,
  left recursion, a callee the compiler declined) goes to the walk's own producer through one bridge
  op, so those shapes behave exactly as before.
- `Grammar.parse`, `<x>*` / `<x>+` (the walk's possessive scan is now one function both engines call),
  `~` goal matches and the `<.ws>` of a `rule` compile too.
- A run of ratcheted calls stays flat in memory: a return that leaves no choice point in the callee
  forgets its journal, trail and window.

`bench-grammar-parse-big` goes from 32.5 ms to 25.0 ms (241 M to 180 M instructions) and the real
JSON::Tiny grammar from 45 ms to 40 ms; rakudo takes 74 and 92 ms. That is a quarter, not the 30x the
flat-program prototype showed on scans: a grammar's cost is per subrule (proto ranking is 22% of the
parse, the Match tree's 186,000 allocations another fifth), which the ADR's section 8 breaks down. The
Slice A scan rows keep their wall time.

Differential mode (`MUTSU_RX_DIFF=1`) agreed with the walk on all of `t/` and the roast whitelist. It
now replays the walk alone, compares Match trees recursively, and sets the reduce log aside so an
action does not run twice.
