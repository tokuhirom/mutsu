# The compiled regex engine runs `<$rx>`, `$x` and `<{ }>` interpolation

The second part of ADR-0135's Slice C (#10253) compiles the interpolation atoms. A spliced Regex
value (`<$rx>`, a bare `$rx`) runs in a capture level of its own whose captures are dropped when it
closes, which is what the walk does for it. `$x` of an in-regex `:my` lexical and `<{ … }>` call the
walk's own single-candidate arm, so the two engines share them. A value that closed over its own
scope still takes the walk.

Writing these exposed a gap in the first part. The walk hands the enclosing regex's `:my` lexicals
to an inline sub-pattern. A compiled capture group, quantifier iteration or conjunction branch opened
a level with none, so `$ni` read inside the atom of a `%` quantifier came back empty
(`t/regex/regex-lookaround-bound-param.t`, which landed on `main` after the first part's sweeps).
Those levels now start with the enclosing level's lexicals.

D6's record of code invocations is now order-exact. A code block that matches a regex which holds
code of its own used to log the inner invocations before the outer one, so the walk's replay saw
them out of call order. An invocation is now reserved before it runs and remembers how many events
its run produced.

Across all of `t/` and the roast whitelist (`scripts/rx-decline-survey.sh`), `isolated-group` fell
from 90 to 0, `interpolation` from 55 to 31 (`code-interp` and `qq-interp`) and `code` from 83 to 42
(`** {n}`), and compiled patterns went from 6,319 to 6,492. What is left of Slice C is `** {n}`,
`$( … )` / `@( … )` and `"…$x.meth()…"`.

Two walk bugs both engines share were filed on the way: `<{ … }>` merges the interpolated
pattern's captures into the caller where rakudo discards them (#10417), and `$/` in its body is
empty (#10418).
