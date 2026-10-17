# A variable's type record comes from the frame that binds it

```raku
my Int $n = 1;
sub set-n() { EVAL q[$n = 'no'] }
set-n();   # dies: Type check failed in assignment to $n
```

A named sub captures a program-scope scalar by boxing it into a shared
cell, and a store that reaches the variable by a run-time name (`EVAL`,
`::('$n')`) checks the constraint the cell carries. The cell made for a sub
declared before the variable's declaration ran carried none, so the store
went unchecked (#13746). That cell now carries the variable's type.

The by-name type record a store checks is now read where the variable lives
(ADR-13757 step 4). A variable the running code declares itself, of any
sigil and including a typed parameter, takes its record from the running
frame only. A lookup of any other variable's record ends at the first caller
frame that binds that variable, so a same-named typed variable of a routine
further down the call chain no longer answers for it. A sigilless
parameter's alias of its argument still reads the argument's record.
