# A bare `callsame`/`nextcallee` site remembers its route

A bare call-chain term is resolved by `GetBareWord`. This covers `callsame`,
`nextsame`, `callwith`, `nextwith`, `nextcallee` and `lastcall` written
without parentheses. The resolution walks the whole bareword chain on every
execution: lexical and unit types, the running module's barewords, the type
registry, enum members, term bindings, imported routine aliases and the
function tables. Only after all of those decline does it reach the call-chain
arm. For `my &original = nextcallee` in Method::Protected's method wrapper,
that walk cost about 31,000 instructions per call, more than a tenth of a
protected method call.

The site now remembers the *route* for one registry write generation: this
name fell through to the call-chain arm. It cannot remember the value, because
each call answers afresh. The argument is the one the `IterationEnd` memo
already relies on. Every declaration that could claim the name first is either
a registry write, which bumps the generation, or an `env` binding, which a hit
re-checks under the name and its term key. A hit also re-checks that the bare
name is visible. It then calls the term exactly as the chain's own arm does.

To make this possible, `push_bare_word_value_in` gained a routed form that
reports which arm answered. Its existing callers are unchanged.
