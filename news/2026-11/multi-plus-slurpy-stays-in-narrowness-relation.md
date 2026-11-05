# A `+@` candidate stays in multi dispatch's narrowness relation

A `+@w` candidate called with a non-list argument (`f("x")`) was scored as "unrelated" to its
implicit `Positional` parameter, which dropped it out of the narrowness relation. With a third
candidate of the same positional type and a required named parameter, the topological tiers then
put `+@w` ahead of the plain `Str:D` candidate and `f("x")` picked the slurpy, where rakudo picks
the typed one. The single-argument rule wraps such an argument, so the candidate binds it: it now
ranks like any positional slurpy that swallows an argument, and stays in the relation.
