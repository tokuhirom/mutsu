# A user `~` candidate binds a variable operand's container

A user candidate of `~` (`multi infix:<~>`, as Red exports) keeps `~` at its
core precedence and makes the application a call of the operator, so an
`is rw` candidate binds a variable operand's container, as rakudo does. Red
binds `$_ ~ $suffix` as a query parameter that way (t/089).
