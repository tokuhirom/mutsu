# The regex interpolation pre-pass scans quote state once

Before a dynamic regex pattern is parsed, mutsu's interpolation pre-pass walks it and
splices variable values into the text. At every `<`, `$` and `@` it asked whether the
position sits inside a quoted literal (`'...'`, `"..."` and their Unicode variants), and
each ask re-scanned the pattern from its start, so the pass was quadratic in the
pattern's length.

That cost showed up in the steady state of a parameterized grammar: the longest-token
measurement of a call such as `<block($indent, 0)>` re-interpolates the callee's body
every time. A `RegexQuoteCursor` now carries the quote state forward as the pre-pass
advances. One warm `load-yaml` of the 60-row `bench-yaml-parse-big` document went from
~272M to ~259M instructions (−4.7%; ADR-12917, #12917).
