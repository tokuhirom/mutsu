`POST` phasers now receive an implicit routine result in `$_`, matching explicit
`return` behavior and Rakudo. This fixes routine bodies whose final expression
produces the result without an explicit return.
