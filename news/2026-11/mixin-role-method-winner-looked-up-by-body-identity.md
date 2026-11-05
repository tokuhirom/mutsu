# A remembered role-method winner on a mixin is found without cloning the candidates

A role multi method called on a mixin receiver (`CArray[num64]`'s `ASSIGN-POS` / `AT-POS`)
cloned and filtered the whole `Vec<MethodDef>` of the role's candidates, then re-derived whether
every candidate is decided by built-in type keys, only to look the winner up in the memo that
#16267 added. The memo is now probed with the identity of the candidates' bodies and the
arguments' type keys alone (`mixin_winner_fast_key`); the candidates are cloned and the
key-decided check runs only when the memo misses, which is also the only time an entry is made.

Callgrind slope on a profiling build, second run: fill (`@c[$_] = 1.5e0`) 89.2k -> 83.1k Ir per
store; fill + `.Array` 198.6k -> 189.1k Ir per element. Release wall clock for the 1,000,000-element
Sort::Fast shape (`$c[$_] = @a[$_].Num for ^N`, then `$c.Array`): 17 s + 20 s, against 224 s at the
issue's baseline and 11 s for the whole test on rakudo. Refs #16215.
