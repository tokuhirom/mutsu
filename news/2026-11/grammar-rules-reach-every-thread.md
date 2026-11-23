# A grammar declared on one thread parses on every thread

A grammar's `token`s, `rule`s and `regex`es are methods of the grammar, and a
grammar is world state: rakudo installs it once per process. Since ADR-17216
slice 2, mutsu shared the grammar's class with every thread. Its rules,
however, stayed in the per-thread routine tables, because those tables also
emulate lexical scope for `my sub` and `my token`. So a grammar `EVAL`'d inside
`start` had a class on the main thread but no rules: `G.parse` died with
"No such method 'TOP'". The same held the other way, for a grammar the main
thread declared after a worker had started.

Every rule declared in a package, `proto token` markers included, is now also
appended to one world log. A thread replays the entries it has not seen at the
entry of a call, which costs one atomic load when nothing is new. The
routine-table restore already keeps package-owned rules, so a replayed rule
lasts as long as the declaring thread's own copy. A `my token`, or a rule of a
`my`-scoped package, is lexical and is not published.
