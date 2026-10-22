# `require M <$var>` with only a variable in the list

The undeclared-variable scan only read a `require` import list written as a list,
so `{ require M <$x>; say $x }` (a single bare literal) died with "Variable '$x' is
not declared". The scan now accepts the single-literal form too (#14340).
