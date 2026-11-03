# A shaped array can be initialized from a scalar holding its rows

`my @a[2;2] = $rows` with `$rows` a scalar variable raised
`X::Assignment::ToShaped`, because the array constructor saw the variable's
container instead of the List inside it. It now reads through the container.
PDF::Grammar's ten baseline test files all match rakudo.
