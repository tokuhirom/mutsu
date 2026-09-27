# Every LTM measurement runs the NFA; the walker's measurement mode is gone

mutsu used to have two engines for measuring a declarative prefix: the NFA of
ADR-0125, used only for a `|` branch ranked from a real match, and the ordinary
backtracking matcher run under `LTM_DECLARATIVE_MODE` for everything else — proto
candidate ranking, the `:rule<...>` entry point, `declarative_prefix_match_len`, and any
pattern the NFA builder declined. The two disagreed: the walker honoured `:ratchet` while
measuring, so a proto candidate `token t:sym<a> { [ \w+ 'c' <.ws> ]? 'ab' }` lost to
`token t:sym<b> { 'ab' }` on "abcd", where Rakudo ranks `a` first.

Now every measurement runs the NFA (ADR-0127, #9644):

- A subrule call compiles to a call into the callee's body, compiled once per NFA, and
  the simulation keeps a call stack per thread. The recursion cut is decided on that
  stack, and a live left-recursion activation's seed is read where the matcher would
  read it. The node budget that made large grammars fall back to the walker is gone.
- Nothing is declined: `<::(…)>`, `<&re>` / `<$re>` and a plain method call are fates
  (as in Rakudo), arguments of a subrule call are ignored, `:m` and scoped interpolated
  regexes are measured by nested NFAs, and a rule whose body is re-parsed per call is
  resolved when the measurement reaches it.
- The `:rule<...>` entry point ranks a fate-ended prefix by the fate's position instead
  of putting it in a declaration-order bucket, so `t:sym<a> { 'abc' {} 'd' }` is tried
  before `t:sym<b> { 'ab' }` on "abcd".
- A single non-proto start rule always runs its real match, so a missing `~` goal
  reaches `FAILGOAL` without relying on the measurement's side effects.

The walker's scaffolding went with it: the measurement memo, the recursion stack, rank
reuse, the alternation flag scopes, the `||` measurement helpers, the ratchet and
quantifier-shortcut exemptions, and `MUTSU_LTM_NFA_VERIFY`. `LTM_DECLARATIVE_MODE` now only
guards the NFA's own leaf calls. Pinned by `t/regex/regex-ltm-nfa-entry-points.t`.
