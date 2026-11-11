# `callsame` ranks a child of a parentless built-in class correctly again

The ranking that `nextsame`/`callsame` defer through scores each candidate's
parameter types by MRO depth. #17372 floored that score at 3 so that a role
(whose MRO is just itself) ranks above an unconstrained `Any` parameter. But
the internal MRO of a class the runtime registers without parents, such as
`Pod::Block`, also lacks the `Any, Mu` tail, so `Pod::Block::Named` (2) and
`Pod::Block` (1) were both floored to 3. Pod::TreeWalker's listener test then
had a subclass's `callsame` jump from its `Pod::Block::Named` candidate to a
role's generic `Pod::Block:D` one, so the start events were lost.

Every type now scores as if its MRO ended in `Any, Mu`. A role still scores 3,
and `Pod::Block::Named` stays narrower than `Pod::Block`. Pod::TreeWalker's
five test files match rakudo again.
