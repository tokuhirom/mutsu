# A slice held in a `$` variable writes through to its Array

`my $s = @e[0..1]; $s.map({ $_ = 8 })` now sets `@e` to `[8 8]`. Before,
it died with "Cannot assign to an immutable value". An Array slice is a
`List` of the Array's own element containers. mutsu already handed those
out when the slice was a method's receiver (`@e[0..1].map(...)`) or was
bound with `:=`. A slice assigned to a `$` variable, though, still held
the bare values.

The right side of a `$` assignment written as a list or range subscript
now uses the same element-cell producer (#17401). The value it stores is
a plain `List` of the cells, not a `:=`-bound slice, so assigning the
variable itself still replaces it, as on rakudo. A single-index read
stays a value.
