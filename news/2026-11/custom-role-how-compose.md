# A custom role HOW's `compose` runs when its role is declared

A role declared through a user metaclass subclassing
`Metamodel::ParametricRoleHOW` -- installed by `EXPORTHOW::SUPERSEDE::role` or
a DECLAREd role-kind declarator -- was built with that HOW but never had the
HOW's `compose` override called; only the class path ran a custom `compose`.
The role registration now calls it once, through the VM method-call path,
with the role's type object as the declaration completes (a hoisted forward
shell is skipped, since the in-place registration composes it), and its
`callsame` reaches the native role compose (#18081).
