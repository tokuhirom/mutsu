# A caller's typed variable no longer constrains a routine's store to a same-named one

```raku
my @a;
sub push-it() { EVAL q[@a.push('s')]; @a.elems }
sub typed-caller() { my Int @a; push-it() }
say typed-caller();   # 1
```

A store made by name inside a routine looked up the variable's type record
the same way it used to look up variables before ADR-12529: through the
frames that called the routine. So a caller's unrelated `my Int @a` made
`push-it`'s push to the program's `@a` fail its type check. The type record
now follows the routine's lexical scope like the variable it describes
(ADR-12529 §3, slice 45).
