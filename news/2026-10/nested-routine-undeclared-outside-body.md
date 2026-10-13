# A routine declared in a routine body is undeclared outside it

A `sub` or `proto` declared inside another routine's body is lexical to that
body, so a bare call to it from outside is rakudo's compile-time
`Undeclared routine`:

```raku
sub f { sub helper { 1 } }; say helper()       # ===SORRY!=== Undeclared routine
sub f { our sub helper { 1 } }; say helper()   # the same; GLOBAL::helper() works
```

mutsu's CHECK-time walk collected declarations scope-blind, so any `sub helper`
anywhere in the unit hid the error: the first line failed only at run time and
the second printed `1`. The walk now records a routine declared in a
`sub`/`method`/`proto` body in that body's scope, and only calls inside the body
(before or after the declaration, including from nested routines) are explained
by it. Types and variables stay scope-blind. The walker moved to
`src/runtime/undeclared_routines/scan.rs` (#13111).
