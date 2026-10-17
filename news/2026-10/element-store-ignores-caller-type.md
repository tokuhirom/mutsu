# An element store checks its own variable's type, not a caller's

```raku
my @a;
sub fill() { @a[0] = 'x'; @a.elems }
sub caller() { my Int @a; fill() }
say caller();   # 1
```

A routine's store into an element of a program-scope `@a` or `%h` was
checked against a same-named, typed variable of whichever routine called
it, and threw a type check failure. The same happened for a closure's
store into a variable it captured, scalars included.

An element store into a captured variable puts the variable's container
under its name in the running frame for the duration of the store. The
type lookup took that as "this frame declares the variable", so it walked
on into the callers. It now follows the static link when the frame holds
the program scope's own container. A closure takes the type from what it
captured and stops there (ADR-13757).
