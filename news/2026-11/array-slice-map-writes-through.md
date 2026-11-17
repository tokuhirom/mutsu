# `@a[0..1].map({ $_ = 6 })` writes through to the array

In rakudo a slice of an Array is a `List` of the Array's own element
containers, so a callback that assigns to `$_` writes the Array. mutsu built the
slice from the elements' bare values, so `.map`, `.grep` and `.first` over a
slice silently wrote nowhere (a `for` loop over the same slice already worked,
because it compiles the slice specially).

The subscript-receiver producer (`IndexInvocantRef`) now knows, from the
method name at compile time, whether the method hands its receiver's elements
out (`map`, `grep`, `first`, `deepmap`, `duckmap`, `nodemap`, `kv`, `pairs`,
`values`); for those, a slice of a real Array is the `List` of the selected
elements' cells — the same producer a slice *argument* toward an `is rw`
parameter already used (#13786). A reading call is unchanged and every other
method still gets the plain slice. A slice first stored in a scalar
(`my $s = @a[0..1]; $s.map(...)`) is still a list of values; that is left on
#17401.
