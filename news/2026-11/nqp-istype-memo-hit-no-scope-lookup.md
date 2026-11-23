# `nqp::istype` answers a repeated check without a scope lookup

A repeated type check of a built-in scalar (`5 ~~ Failure`) is answered from
a memo. Before it trusts the memo, the check confirms that the type's name
still means that type in the current scope: a constant or a lexical class
could shadow it. From module code, that lookup misses the lexical
environment and walks the enclosing package chain, about 9k instructions.

`nqp::istype` is handed the type object itself, so a name that shadows the
type would already have produced a different operand. A hit from
`nqp::istype` no longer asks. Filling the memo still requires the name to
mean its own type, so by-name checks (`~~`) never read an answer computed
while the name was shadowed.

JSON::Fast's `parse-numeric` runs `nqp::istype($result, Failure)` for every
number. The 100-record SPDX decode went from 60.30M to 59.01M instructions
(−2.1%, callgrind). `t/vm/nqp-istype-type-object-memo.t` pins the answers,
including a lexical `Int` class and a `constant Str` beside the core types.
