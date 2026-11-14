# The sequence operator chains like rakudo's

`...` binds more loosely than the comma and is list associative, so
`1 ... 3, 10 ... 20` is `1 ... (3, 10) ... 20`. mutsu bound it more tightly
and repaired the result piecemeal, which went wrong in three ways:

- In a parenthesized list, a nested `a ... b` was flattened to a plain
  `(a, b)` pair, and the runtime guessed at segments from evaluated values.
  `("A"..."C", "a"..."c", 0...2, "+")` never ended: it counted from 0 towards
  `"+"` until memory ran out. HTTP::Tiny's base64 alphabet,
  `('A'...'Z', 'a'...'z', 0...9, '+', '/')`, ran into this, so `t/requests.t`
  and `t/url-parsing.t` aborted with "memory allocation failed".
- In an assignment, only a sequence in the last position took its seeds;
  trailing values and chains stayed separate items. `my @d = 0...2, 5` was
  `[(0 1 2) 5]` instead of `[0 1 2 5]`.
- `infix:<...>` with three or more operands did not follow rakudo's rule.

Every one of these forms is now lowered to one `infix:<...>(g0, g1, ...)` call,
which follows rakudo's rule:

- A number or string that starts an operand ends the previous segment short
  and also seeds the next segment.
- A closure or `*` endpoint is consumed by its segment.
- The last operand's values follow the last segment.

So `(1...3, 10...20)` is `1 2 3 10 17 20`, and `(0...2, 5...4)` is `0 1 2 4`.
An infinite segment keeps the result lazy.
