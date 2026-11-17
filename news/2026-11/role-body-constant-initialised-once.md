# A role body's `my constant` is initialised once

A lexical `my constant` in a role body is now initialised exactly once, when
the role is declared, as Rakudo does (#16255). Before, the initializer could
run three times: in the forward-reference shell the unit registers before its
statements run, at the declaration, and again at the first composition (or at
the first `does`/`but` mixin), so a side-effecting initializer was observed
several times and wrote into lexicals that were not yet the unit's real ones.

The declaration-time pass now records the binding it computed
(`Registry::role_evaluated_constants`), every composition path (a class's
`does`, a role composed through another role, a runtime mixin) puts that
binding back instead of evaluating again, and the hoisted forward-reference
registrations (of the role itself and of a class composing it) leave the
initializer alone. A role declaring a body constant is also a BEGIN-time
effect of its unit, so the initializer sees the unit's lexicals in their
static state. Constants that call a routine of the role body still wait for
the first composition, as before.

Found along the way and filed as #17833: such a constant also leaks into the
scope that declares or composes the role.
