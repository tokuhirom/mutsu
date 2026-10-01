# Code in a `%` quantifier or a `&` branch runs in the compiled regex engine

The compiled regex engine (ADR-0135) used to decline a pattern with code
(`{ … }`, `<?{ … }>`, `** { … }`, a `$x` lexical) inside a separated
quantifier's atom or separator (`separator-code`) or inside a `&` conjunction
(`conjunction-code`), and sent it to the tree walk. The compiled form matches
each iteration, and each conjunction branch, in a capture level or nested run
of its own, which hid the enclosing captures from the code.

Such a level is now an *inline* level: it starts from the enclosing level's
view. For a separated quantifier that view is the captures before the
quantifier, then the iterations folded so far, with the one in progress folded
into the atom's slots (or the separator's). So Net::Whois's octet check
`[ (\d ** 1..3) <?{ $/[*-1][*-1] < 256 }> ] ** 4 % '.'` reads the octet being
matched. A conjunction's first branch reads the enclosing captures. Each later
branch's nested run is seeded with them and with the earlier branches'
captures, as rakudo's single cursor has them. Both reasons now read zero in
`scripts/rx-decline-survey.sh`.

Comparing the two engines turned up four walk bugs, all fixed against rakudo:

- A quantifier inside a `[ … ]` that followed a capture folded into the wrong
  slot. Its fold start was relative to the group's own captures.
- The atom after a separator did not see that separator.
- Code in a separator saw nothing of the chain, in both the backtracking and
  the ratcheted scan.
- A conjunction's later branch saw neither the enclosing captures nor the
  earlier branches'.

Two further differences from rakudo, shared by both engines, are filed: zero
iterations of a separated quantifier drop its positional slot (#10534), and
code in a nested separated quantifier sees the inner fold beside the outer slot
(#10535).
