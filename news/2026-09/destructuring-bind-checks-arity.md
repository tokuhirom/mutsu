# A destructuring bind checks its arity

`my (...) := LIST` binds like a signature, and now it counts like one too
(#9763). A positional count outside the declarator list's required..max range
dies with rakudo's wording — `my ($p, $q) := (1,)` is "Too few positionals
passed; expected 2 arguments but got 1", `my ($a, $b?) := (1, 2, 3)` is "Too
many positionals passed; expected 1 or 2 arguments but got 3", and a slurpy
turns the bound into "expected at least N arguments but got only M". Before,
mutsu bound whatever was there and left the missing targets `Nil`.

An optional element the RHS does not reach now takes an optional parameter's
default: its `= default` expression, else the constraint's type object
(`Mu` when untyped), where mutsu used to leave `Nil` and ignore the default.

The guard is two `EXISTS-POS` probes emitted by the parser's lowering, so a lazy
RHS feeding a slurpy (`my ($a, *@r) := 1..*`) is not reified by the check. List
assignment (`=`) keeps its lenient behaviour.
