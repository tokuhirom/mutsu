# `@`/`%` for-loop parameters are isolated across sibling `start` blocks

`for [1,2],[3,4] -> @c { start { ... @c.sum ... } }` made every spawned block see the
last iteration's `@c` (`(3 3)` instead of `(3 7)`). The for loop now records an
`@`/`%` loop parameter as a parameter-bound aggregate, the same provenance rule
routine parameters already use, so it stays off the shared bare-name lane
(ADR-0023 follow-up). Pinned by `t/routines/signature/for-aggregate-param-start-sibling-isolation.t`.
