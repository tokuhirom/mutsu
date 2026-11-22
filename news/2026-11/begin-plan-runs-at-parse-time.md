# `BEGIN plan N` runs when the parser reaches it

A top-level `BEGIN` that calls a routine an earlier `use` imports
(`use Test; BEGIN plan 3;`) used to wait for the BEGIN-time prologue. Every
`use` after it then loaded in the prologue too (ADR-17558 §7.1's transitional
rule). It now runs at parse time, as on rakudo (ADR-17558 §7.16).

The parser records the `use` statements it loaded at parse time. The `BEGIN`
runs with the ones that import the routines it calls ahead of it, so:

- `BEGIN plan 3; 1 +;` prints `1..3` before the syntax error;
- a later `use` still loads at parse time, after the plan;
- `BEGIN { plan 1; skip-rest; exit }` ends the program before the rest is
  parsed, with END phasers and its status.

Found along the way: a module loaded at parse time runs no END phaser when a
later syntax error fails the compile (#18918).
