# Pod::EOD can collect Pod from EVAL units

`Pod::EOD` uses `EVAL` to compile source containing Pod and then reads the
resulting `$=pod` array. mutsu previously parsed a bare `$=pod` as an
assignment to an anonymous state variable and did not collect declarator Pod
blocks created inside the EVAL unit.

The parser now leaves `$=pod`-style twigil reads to the variable parser, while
preserving `$=` assignment syntax. EVAL compilation also establishes its Pod
and declarator-documentation state before executing the unit and restores the
caller state afterward.

Pinned by `t/lang/pod-eval-return.t`. `Pod::EOD` 0.1.1 moves from red (0/1
baseline files, 0/4 assertions) to green (1/1 files, 4/4 assertions).
