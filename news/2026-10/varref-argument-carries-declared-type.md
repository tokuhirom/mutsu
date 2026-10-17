# A variable passed as an argument keeps its own declared type

```raku
my Int $t = 1;
sub rw-target($x is rw) { $x = 's' }
sub store() { rw-target($t) }
sub caller() { my $t; store() }
caller();   # Type check failed in assignment to $t
```

When a variable is passed as an argument, the parameter binder and the
multi dispatcher look up the variable's declared type. They did that by
name after the callee's frame had been created, so the lookup started in
the wrong frame and walked through the routines that made the call. Here
it reached `caller`'s untyped `$t` instead of the program's `Int $t`, and
the store through the `is rw` parameter went unchecked.

The call site now resolves the type in the variable's own frame, where it
is evaluating the argument anyway, and the argument carries the answer.
A routine's untyped local answers from the compiled code without a lookup
(ADR-13757).
