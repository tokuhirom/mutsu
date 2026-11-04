# A named sub of an EVAL'd unit captures the unit's variables

`RegisterSub` gave a named sub's free variables a lexical capture cell (ADR-0024) only
outside an `EVAL`. A sub declared at the top level of an `EVAL`'d unit therefore resolved
the unit's variables by name through its caller's env chain, and a store that a nested
`EVAL` made to such a variable was boxed into every saved call frame that owned the name -
including a caller's own, same-named variable - while the unit's own slot never received
the cell. `EVAL Q[my $x = 1; sub set-x() { EVAL q[$x = 'str']; $x }; sub c() { my $x = 5;
set-x() }; c(); say $x]` printed `1` (raku: `str`), and panicked on the env/slot invariant
assertion of a debug build (#16371).

An `EVAL` run now has its own capture bucket key (`UNIT<eval N>`, `eval_unit_capture_key`),
and the named subs at the unit's top level capture the unit's variables in it, as mainline
subs do in the program's bucket.

Test: `t/routines/call/eval-unit-sub-captures-unit-variable.t`.
