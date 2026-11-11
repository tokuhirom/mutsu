# Enclosing constants are lexical reads, and an `our`-store miss skips the visibility gate

Two context-dependent costs dominated App::Moneymoor's
`t/15-invariants-property.rakutest` property soak (#17265).

A module's routine reading the module's own `my constant`
(`my constant DAYS-IN-MONTH = (...)`, read as `DAYS-IN-MONTH[$m - 1]` from
`our sub valid-date`) compiled to the run-time bareword resolution chain.
That chain re-derives the answer on every read and its cost grew with the
calling context: about 0.5 ms per read inside the budget derivation, against
a few microseconds standalone. A constant declared in an enclosing scope now
compiles the way an enclosing `my \x` already did, as a by-name read of its
term key that the closure capture records, so it costs what any captured
lexical costs.

`get_our_var` ran the module-visibility gate of #13942 before looking the
key up. For a package that a loaded module granted but the running code did
not merge, that gate ends in `module_name_hidden_here`, whose last check
builds `GLOBAL`'s whole stash. A bareword inside a module routine probes
`Pkg::name` in the package store for every package the routine could run
in, so each such bareword (`CategoryPeriod.new`) paid a full `GLOBAL` stash
build, about 1.4M instructions. The store is only ever read under the key
itself or its unmerged-store spelling, so when it holds neither the answer
is "no such variable" whatever the gate decides; the lookup now checks that
first and runs the gate only for a key that exists.

The soak went from 173 s to 65 s on a 4-core box (rakudo: 19 s; release
build). The remaining gap is spread over list-destructuring declarations,
`--> List` return types and the general call floor.
