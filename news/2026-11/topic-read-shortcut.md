# A `$_` read goes straight to its binding

Every `with` / `given` body, statement-modifier loop and topic method call
reads `$_` with a by-name `GetGlobal`. The topic took the opcode's plain-env
shortcut, but only after the probes that run ahead of it: the routine-nested
free-variable cache, the rw-return and magic-name checks, the attribute
lookups and two package-qualification tests, none of which can answer for
`_`.

A topic read now checks the few gates that can matter for it (an rw-return
context, an atomic `$_`, sigilless attributes, escaping `our` captures) and
reads the env binding directly. A `with` body testing its topic four times
went from 21.3k to 20.3k instructions per call; bench-json-fast's JIT row
dropped 2.8% and method-call's 1.6%.
