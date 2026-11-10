# EVAL lists every undeclared routine, as "Undeclared routines:"

`EVAL 'floobody doop'` reported only `Undeclared name: doop`. A lowercase bareword is a
routine to rakudo, and rakudo lists every undeclared routine of the unit (calls included),
sorted by name, under `Undeclared routine:` / `Undeclared routines:`. The EVAL name check now
collects all undeclared barewords and undeclared calls and uses that wording when none of them
is type-like. Jupyter::Kernel `t/02-sandbox.rakutest` test 13 matches raku.
