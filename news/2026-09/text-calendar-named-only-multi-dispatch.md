# Text::Calendar no longer loses defaulted multi candidates

`Text::Calendar`'s `calendar-year()` has a defaulted positional multi candidate
alongside a named-only candidate. mutsu's exact-arity fast path returned the
named-only candidate immediately, so the defaulted positional candidate was
never considered and the module's sanity test died while stringifying its
missing year.

Multi dispatch now lets an exact named-only candidate compete with flexible
candidates, while preserving exact-arity precedence for ordinary positional
multis. Optional positionality is also ignored when both candidates declare
named parameters, matching Rakudo's declaration-order tie.

Pinned by `t/routines/dispatch/multi-flexible-arity-named-only.t`.
`Text::Calendar` moves from `partial` (1/2 baseline files) to `green` (2/2,
13/13 assertions).
