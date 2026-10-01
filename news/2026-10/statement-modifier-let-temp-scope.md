# `let`/`temp` behind a statement modifier resolves at the enclosing block

A statement-modifier `for`/`while` opens no block in Raku, so `temp $a = 5 for ^1`
now stays live until the enclosing block exits instead of being restored after
each iteration, and the enclosing block's save frame sees saves made inside the
modifier loop. A `let` behind a compile-time-constant `if` modifier
(`let $e = 9 if 1; Nil`) no longer gets a private frame of its own, so it rolls
back with the enclosing block like the run-time-condition form already did.
