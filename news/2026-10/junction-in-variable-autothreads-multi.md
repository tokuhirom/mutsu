# A Junction held in a variable autothreads over a multi

`g($x)` with `Mu $x` holding a Junction died with "Cannot resolve caller" when `g`'s
candidates take `Any`/`Str`: the autothread check looked at the variable's varref wrapper
rather than the Junction inside it, so only a literal junction argument threaded (#13148).
