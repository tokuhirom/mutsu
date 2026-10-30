# A nested `sub` shadows an imported constant term of the same name

`phi(...)` after `sub phi` in a routine, with a module-exported `constant phi`,
parsed as a CALL-ME on the imported term and died with `No such method
'CALL-ME' for invocant of type 'FatRat'`. The imported-term parser now declines
a name followed by `(` when a sub of that name is declared in an enclosing
scope. Found with Math::NumberTheory (Legendre's formula uses a nested `phi`
next to its Constants import); its test suite now matches raku.
