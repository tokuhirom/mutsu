# A compound assignment as the tail of an `is rw` method returns the container

`method table(Mu \type) is rw { $!table //= ... }` (Red's `^table`) returned the
assigned value instead of the attribute's container, so `Bla.^table = "x"` died with
"Cannot modify an immutable Str". The tail of an `is rw` routine that is a compound
assignment to a variable now runs the assignment and yields the variable's container,
as a bare `$!table` tail does. Red `t/23-metamodel-model.rakutest` now gets past the
`CREATE TABLE` and `^table` assignment (#15246).
