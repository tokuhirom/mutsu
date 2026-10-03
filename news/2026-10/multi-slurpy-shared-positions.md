# A slurpy no longer widens a multi candidate when it binds nothing

`multi a(Str:D $n, *@p)` now beats `multi a(Any:D $r)` for `a('c')`, as in
Rakudo. Before, the type distance charged an unbound slurpy positional a flat
penalty, so the wider `Any:D` candidate won. For MVC::Keayl this made
`url-for('cart')` pick the `Any:D $record` method and recurse until the stack
ran out.

This follows Rakudo's `is_narrower` rule. It compares only the positional
parameters that both candidates declare. Slurpiness counts only after those
tie, and it comes before the named bind check. A slurpy that does swallow an
argument still pays the penalty, so `multi f($x)` keeps beating
`multi f(*@a)` for `f(1)`. Sub and method multis both follow the rule
(#11045).
