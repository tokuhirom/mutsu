# `.^roles` no longer lists the class parent of a composed role

A role's `is P` (P a class) is recorded next to its `does` parents, so
`Consumer.^roles` for `class Consumer does Punned` with `role Punned is Parent`
reported `Parent` as a role. The `.^roles` walk now keeps only parents that are
roles, matching Rakudo (`Other Punned`). Closes #12504.
