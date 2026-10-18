# A `my class` body inside a routine reads the program's lexicals

`my $read = "outer"; sub k2 { my $o; my class P2 { $o = $read }; $o }`
returned `(Any)` (#13675). Each class-body statement compiles in a chunk of
its own, and that chunk did not know the lexicals visible where the class is
declared, so it qualified `$read` as the package variable `$P2::read`. The
declaring routine then had no free variable `$read`, captured nothing for it,
and the run-time fallback to the bare name read the env, which held no live
value.

The chunk now keeps every lexical visible at the declaration bare, as it
already did for the class body's own `my` variables. An undeclared variable in
a class body is a compile error in rakudo, so a name that compiles there is an
outer lexical, and it becomes a free variable of the declaring routine like any
other (ADR-12529 phase 3, slice 35).
