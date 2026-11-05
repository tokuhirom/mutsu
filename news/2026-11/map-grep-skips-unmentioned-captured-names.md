# A map/grep callback no longer installs the program's names it never mentions

A `map`/`grep` callback carries the program's names in its captured environment:
once a module such as Cro is loaded that is dozens of type, enum, qualified-class
and sigilless-term names, plus a `__mutsu_sigilless_readonly::` marker for each
term. The inline loop probed, installed and removed every one of them in the
consuming frame for every element (`enter_inline_loop_env`), so `@a.map(* * 2).grep(* > 4)`
cost 5.5x the bare program with Cro loaded (#7817).

The loop plan now leaves out a captured type-shaped name (`Pt`, `Cro::HTTP`), a
sigilless term (`\size`) and its readonly marker when the callback body cannot
reach the name any other way: it neither mentions the name (a free variable or a
string constant of its ops) nor contains a nested closure or routine, a
declaration, or a by-name lookup (`EVAL`, a symbolic or pseudo-package access).
Release-equivalent callgrind (profiling build, slope over 1,000 to 3,000
iterations of `my @a = 1..10; @a.map(* * 2).grep(* > 4).elems`): with Cro loaded
1,257,801 -> 655,027 Ir per iteration (-48%), bare 227,337 -> 225,450 (-0.8%).
