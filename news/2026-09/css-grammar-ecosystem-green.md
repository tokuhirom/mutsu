# CSS::Grammar is green in the ecosystem parity ledger

`CSS::Grammar` v0.4.3 now passes all six of its Rakudo-baselined test files
under mutsu: 1,056/1,056 assertions with no distribution changes.

The interpreter fixes cover grammar action replay across backtracking, zero-width
silent-rule actions, ordered alternatives with optional branches, proto-token
dispatch and LTM ordering, CSS-style rule whitespace and comments, inline
case-insensitive bracket scopes, and octal character-class ranges.

Pinned by the focused grammar and regex regressions under `t/grammar/` and
`t/regex/`.
