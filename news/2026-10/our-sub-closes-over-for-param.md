# An `our sub` in a `for` body closes over the loop parameter

`package P { for 1..2 -> $i { our sub g { $i } } }; say P::g()` printed `(Any)`:
a single pointy-block loop parameter has no local slot (the `ForLoop` opcode
binds it by name in the env), so the per-activation free-variable alias
mechanism of routine-nested subs (mutsu#9111, #10559) skipped it and the
escaped sub read nothing after the loop. The alias now also covers such a
slotless loop parameter (`LexSubFreeAlias::env_param`): each execution of the
declaration boxes the env binding in place and hands the cell to the sub, so
the call after the loop sees the last iteration (`2`, as in Rakudo) and each
iteration's `&g` keeps its own `$i` (mutsu#10512). The same shape at GLOBAL
mainline goes through a different capture path and is tracked as #10647.
