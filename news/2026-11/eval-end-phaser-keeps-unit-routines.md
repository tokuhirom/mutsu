# An EVAL unit's END phaser keeps the unit's routines

`EVAL q[sub foo { ... } END { foo() }]` died at program exit with `Unknown function:
foo`: the unit's routines are lexical to it and leave the registry when it ends, but
its END phasers run at exit. An END installed inside an EVAL unit now carries the
routines the unit declared and re-installs the missing ones for the duration of its
body. Two more `t/` files join the `EVAL(slurp($file).AST)` ratchet (#16356).
