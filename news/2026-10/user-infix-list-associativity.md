# `is assoc<list>` user operators take the whole list in one call

OneSeq defines `my sub infix:«>>>»(**@iterables) is assoc<list> is equiv(&[~])`.
Its test failed in three ways, each a separate gap:

- **The trait order lost the associativity.** A precedence trait (`is equiv`,
  `is tighter`, `is looser`) stored its own name in the associativity slot as a
  placeholder, overwriting an `is assoc<list>` written before it. It now leaves
  an explicit associativity alone.
- **`a op b op c` folded pairwise** when the operator had a declared
  precedence. That operator is parsed at its own level by a separate helper,
  `try_custom_infix_at_level`, which ignored associativity. The helper now
  delegates to the shared custom-infix application, which already collects a
  list-associative run into one `op(a, b, c)` call (and handles right/non/chain
  associativity and trailing adverbs).
- **`[op] a, b, c` folded pairwise.** The reduction gains a `List`
  associativity, so a user `is assoc<list>` operator is called once with every
  element, as in rakudo.

A related longest-token gap is fixed on the way. With `is equiv(&[~])`,
`1 +++ 2` was read as `1 + (++2)`, because the tighter additive layer took the
`+` prefix of the longer user symbol. The additive and multiplicative layers now
stop when a longer declared user symbol starts at that position at a looser
level, and the operator's own level takes the whole token.

All 21 of OneSeq's tests pass under mutsu. Two smaller side findings were
filed: #11323 (the same longest-token check at the `**` level) and #11324
(`Routine.prec`).
