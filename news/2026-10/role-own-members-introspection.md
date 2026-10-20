# Role type objects answer `.^methods` and `.^attributes`

`R.^methods` and `R.^attributes` on a role type object used to return an
empty list, because the class-oriented walks behind them only looked in the
class registry. They now report what the role itself declares, like Rakudo:
its own methods and submethods (no generated accessors, which only a composing
class gets) and its own attributes. Members a role takes from a `does` /
`also does` parent stay on that parent, so `B.^roles[0].^attributes` lists
them.

A run-time mixin's `.^attributes` (`5 but R`, `True but B`) now also lists the
mixed-in roles' attributes ahead of the base type's, each role's own before
those it composes from its parents; `:local` gives just the mixed-in roles'.
