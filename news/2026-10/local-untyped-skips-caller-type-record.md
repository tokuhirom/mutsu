# A caller's typed variable no longer constrains a routine's same-named one

```raku
my @a;
sub push-it() { EVAL q[@a.push('s')]; @a.elems }
sub typed-caller() { my Int @a; push-it() }
say typed-caller();   # 1
```

A store looked up the variable's type by name, and when the running routine
had none recorded, that lookup went on into the routines that called it. A
caller's unrelated `my Int @a` therefore type-checked `push-it`'s push to the
program's `@a`. The compiler now records which scalar variables a routine
declares without a type (its `my $x` and untyped parameters), and a store
into one of them checks nothing. The type record of any other variable
follows the routine's lexical scope like the variable it describes, not the
callers (ADR-13757).
