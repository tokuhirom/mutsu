# RakuAST: equal terms in a modified statement and its modifier keep their own ranges

In `$n = $n if $n` and `$n = $n.abs if $n` the assignment source and the modifier's condition are
equal terms, and the conversion gave the source the condition's range (22-24 instead of 16-18) or
left the condition without one. Three changes, all matching `raku` 2026.09 on the measured cases:

- a statement with modifiers is converted as the statement (up to its first modifier) and each
  modifier in its own range, so a lookup by shape only sees the terms of that part;
- a term-origin slot hit outside the statement being converted is not trusted: the table is keyed
  by address, and a copy the conversion made can sit where a term once did;
- a modifier operand the parser placed no range for takes the text after the keyword.

Left: `$n = $n.abs if $n.WHAT` (the condition's invocant and name), filed as #16656.
Test: `t/rakuast/rakuast-modifier-equal-terms-origins.t`.
