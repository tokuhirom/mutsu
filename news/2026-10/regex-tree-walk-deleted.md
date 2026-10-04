# The regex tree walk is deleted

mutsu now has one regex engine. Every match, whether a `~~`, a scan, `:ov`/`:ex`, a grammar parse
or an LTM measurement, runs the compiled backtracking program of
[ADR-0135](../../docs/adr/0135-regex-compiles-to-a-backtracking-program.md). The recursive walk
over `RegexPattern` that was the engine until Slice A is gone, about 5,000 lines with it. That
deletion was the ADR's completion criterion (D7, #10255).

Before the deletion, the last shapes the compiler still declined compile:

- `<::(EXPR)>` symbolic calls;
- multi rules, and protos called under `:i`;
- `%<name>=` hash aliases and counts past `u32`;
- a backreference in a separated quantifier;
- a frugal separated quantifier under ratchet with code;
- `:m` groups with code;
- a stray `~`, which now raises rakudo's "Unrecognized regex metacharacter ~" instead of
  panicking.

A pattern the compiler declines (none in the test suites or the roast whitelist) now raises "This
regex construct is not implemented by the regex engine" rather than switching engines.

The walk's switches and counters go with it:

- `MUTSU_RX_VM=off`;
- the `MUTSU_RX_DIFF=1` differential mode (D6);
- the `regex-walk:` and `subrule-stream` lines of `MUTSU_VM_STATS`.

The differential corpus that compared the two engines is now `tests/regex_engine_corpus.rs`. It
pins each program to the output both engines agreed on.
