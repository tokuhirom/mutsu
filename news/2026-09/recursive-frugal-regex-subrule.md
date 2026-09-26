# Recursive frugal `regex` subrules match correctly and in polynomial time

`grammar { token TOP { <A> }; regex A { '{' [ <A> | . ]*? '}' } }` failed to
parse `{{a}{b}}`, and `.subparse` stopped at `{{a}`: each iteration of the
frugal loop took `.` for a nested `{` before trying `<A>`, although `<A>` wins
the LTM ranking. On `'{' ~ ('{ab{c}d}' x $n) ~ '}'` it also took 13 s at
`n = 6`. The `token` form was fixed by #9579; the `regex` form had three
causes of its own:

- **Frugality inverted the per-iteration candidate order.** The expansion of a
  non-ratcheted `*`/`+` over an alternation iterated a frugal loop's
  candidates lowest-priority first. Frugality only changes how many
  iterations are preferred, not which candidate one iteration tries first.
  That expansion (`walk_quant_alt`) is gone: those quantifiers now use the
  demand-driven `walk_quant_group_candidates` walk that group atoms already
  used. It tries each iteration's candidates highest-priority first, and it
  asks for the next one only after the continuation rejects the current one.
- **A recursive subrule call was never streamed.** A `<subrule>` call is
  streamed (ADR-0073) only when its call cone provably cannot re-enter the
  rule. Any recursion at all declined it, so every nested `<A>` enumerated its
  whole end set before the caller could try the first. The stream decision
  now accepts a rule whose own-name calls all come after something that
  consumes input (`regex_left_call_graph`). Each nested call then runs under a
  different left-recursion key, and the stream's runtime escapes still cover
  a code block that re-enters the key by hand.
- **Measurement walked every path through the loop.** Ranking a `|` measures a
  branch by walking it in declarative mode, which collects every end. The
  quantifier expansion now skips a `(position, iteration count)` state it
  has already expanded during a measurement. Nothing runs during a
  measurement, so a repeated state can only report the same ends again.

`n = 8` now takes 0.1 s instead of not finishing, and `n = 32` about 2 s in a
release build. That growth is polynomial, not exponential, but it is still
far from Rakudo, whose declarative prefix is a precompiled NFA. Pinned by
`t/regex/recursive-frugal-regex-subrule.t`.

Closes #9596.
