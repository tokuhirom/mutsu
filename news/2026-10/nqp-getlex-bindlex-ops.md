# nqp::getlex / bindlex and the context-relative lookups

`nqp::getlex`, `nqp::bindlex` (and their `_i`/`_n`/`_s` forms) now read and write a
lexical of the running frame by a runtime name. A frame that names a lexical this way
mirrors its slots into `env` like an `EVAL` frame, drains a `bindlex` write back into
its own slots, and treats every plain scalar it declares as mutated, so a closure
capturing the lexical shares one cell with it (`my $x = 5; my &g = { $x };
nqp::bindlex('$x', 9); g()` answers 9, as in Rakudo).

`nqp::getlexrel`, `nqp::getlexreldyn`, `nqp::getlexrelcaller` look a lexical up in a
captured context, and `nqp::curlexpad` answers the current frame's lexpad.

Still open under #11498: `getlexouter`, `getlexcaller`, `ctxouter`, `getlexref_*`,
`lexprimspec`, `bindlexdyn`.
