# A regex smartmatch pays less per execution

`$line ~~ / 'code=' \d+ /` spends most of its time outside the regex engine:
`bench-regex-match` measured about 12k instructions per `~~`, of which the
match itself (prefilter and engine) is about 2.4k. Four of the per-execution
costs were pure functions of things that do not change between executions:

- The names the RHS's regex literals and substitutions can read by name, which
  the op publishes before matching, were recomputed every time by re-scanning
  the RHS's ops and re-parsing each pattern. They are now computed once per
  compiled chunk (`CompiledCode::smartmatch_rhs_names`). Only the names are
  memoized; the values are read on every execution.
- The "did the RHS modify the topic?" check compared the topic with the left
  operand through `values_identical`, i.e. a Str subject by its text, even
  though the topic still held the very value the op had installed. Word
  identity is now tested first.
- `$/` was found by interning `"/"` twice per execution.
- Capturing a regex literal's closure asked the `MONKEY-SEE-NO-EVAL` pragma
  before checking whether the pattern even contains `<{`.

A failing literal match (`bench-regex-match`'s line 5 shape): **12.2k -> 10.6k
instructions per `~~` (-13.5%)**. Every line of `bench-regex-match` is about
10-15% faster locally. Raku++ v5.9.0 still spends about a fifth of mutsu's
per-match overhead (#19296).
