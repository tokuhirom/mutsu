# A class-body write reaches the outer lexical, not a package variable

`my $z = 1; class E { method m { $z } }; class F { $z = 4 }; say E.new.m`
printed `1`; Rakudo prints `4` (#11086).

A class body compiles each statement as a separate chunk, with the class as the
current package. That chunk did not know the declaring scope's lexicals, so
`$z = 4` compiled to a write of `$F::z`. The registration then copied the new
value over the outer `$z` binding by value. `E`'s method had captured `$z` as a
shared cell, and that copy replaced the cell, so the method kept reading the
old value.

The chunk compiler now treats the declaring frame's lexicals as lexicals, and
leaves them unqualified unless the body declares the same name `our`. The
write goes through the outer binding's cell, so every capture sees it.
