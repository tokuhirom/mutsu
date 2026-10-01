# INIT and CHECK in class, role and package bodies run before the mainline

An `INIT` or `CHECK` phaser written in a class, role or package body, or in a
method or `sub` of one, used to run when that body ran: after the mainline
statements ahead of the declaration, and once per composition in a role
(`role R { INIT say "i" }` composed twice said `i` twice). A `sub` the BEGIN
prologue took ahead of the mainline ran its `INIT` on each call instead of
once.

Such a phaser now joins the unit's own INIT/CHECK sequence, as on Rakudo:

```raku
say "run"; class C { CHECK say "c1"; INIT say "i1" }; CHECK say "c2"; INIT say "i2"
# c2 c1 i1 i2 run
```

A phaser of a class or package body still sees the body's lexicals: it
re-enters the package through the same static store the body's methods use,
and the package is composed in the BEGIN prologue so the store exists when the
phaser runs (ADR-0134, #10552).
