# A parameterized role's multi methods are served from the resolution cache

Inside a role with a named parameter (`role R[::T = Any, :&cmp = &infix:<cmp>]`),
`::?CLASS` is substituted with the role's pun name, which spells its arguments
(`R\0default[Any,:cmp(infix:<cmp>)]`). The gate that decides whether a multi
method's winner is a function of the argument types read the `(` in that name
as a coercion type and refused the whole candidate set, so every call re-ranked
every candidate from scratch.

A registered class or role name is now nominal whatever it spells. A three-candidate
`push` on such a role went from ~178k to ~60k instructions per call, which is the
`@a.push` path of Concurrent::PriorityQueue (#15786).
