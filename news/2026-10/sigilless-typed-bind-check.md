# A typed sigilless declaration checks the value it binds

`my UInt \startxref = $.offset` failed with "Type check failed in assignment
to $startxref; expected UInt but got Int (263)" when the right-hand side was a
variable or an accessor. A sigilless declaration binds its initializer, and
the compiler hands the bound variable to the type check wrapped in a `VarRef`.
The assignment-form check matched that wrapper against the constraint. A
subset such as `UInt`, or a user `subset`, could never match it.

A typed sigilless declaration now uses the bind-form check, whether it is
spelled `=` or `:=`. That check looks through the wrapper for subsets and user
types as well as for the built-in ones. A mismatch is reported as
`X::TypeCheck::Binding`, as in Rakudo. PDF 0.6.17's `t/write-pdf.t` now passes
all 16 tests, as it does under `raku`.
