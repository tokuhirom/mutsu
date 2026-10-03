# A role's `where`-constrained multi candidates survive a class's own multi

When a class composed a role and declared its own candidate of one of the
role's multi methods, mutsu decided which role candidates the class's one
replaced by comparing only parameter types and literal values. So
`multi method step(Str $id where $_ ~~ 'stop')` in the class replaced
*every* `step(Str $id where ...)` candidate of the role, and calls for the
other states fell through to a catch-all candidate.

The `where` clause is now part of that comparison, as it is part of the
dispatch signature in rakudo. A class candidate replaces a role candidate only
when their `where` clauses are the same as well.

Found through the ecosystem roulette: DSL::FiniteStateMachines' state roles
declare one `choose-transition(Str $stateID where $_ ~~ '...')` per state, and
its machines override a few of them.
