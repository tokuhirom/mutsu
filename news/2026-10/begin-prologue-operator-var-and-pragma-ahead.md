# A nested BEGIN behind an operator variable or a pragma runs at BEGIN time

A `BEGIN` nested in a routine or block runs once, before the unit's run time,
even when its scope declares an operator code variable or a lexical pragma
ahead of it (ADR-0134 §7, #10472):

```raku
sub f { my &infix:<xx2> = { $^a ~ $^b }; BEGIN say "b" }; say "m"   # b, m
sub g { use strict; BEGIN say "b" }; say "m"                         # b, m
```

Both used to print only `m`. And because the first BEGIN that could not be
lifted kept every later nested BEGIN of the unit on the old path too, one such
scope also delayed all the BEGINs after it.

Every operator code variable of the scopes around a lifted BEGIN now gets a
static cell, as other lexicals do. The operator's syntax and a symbolic lookup
both reach the variable without naming it where a scan could see. The lifted
body's block repeats the scope's pragmas, as it repeats its imports. A pragma
whose mode the block restores (`strict`, `newline`) or that is a no-op in mutsu
is always repeated. One that changes the declarations compiled after it (`use
fatal`, `use variables`, `use dynamic-scope`) is repeated only when the BEGIN
copies nothing that precedes it in its scope. `use lib`, `use if` and `use
attributes` still keep the BEGIN on its old path.
