# Prefix `~` names the uninitialized value it stringifies

`~%h<k>`, `~@a[3]` and `~$x` warned `Use of uninitialized value of type Any in
string context.` with no variable. The compiler now records the operand's
rakudo spelling (`%h{'k'}`, `@a[3]`, `$x`) against the `StrCoerce` op, in the
same table string interpolation uses, and the warning names it as Rakudo does.
