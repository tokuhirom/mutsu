# An rw method whose tail is `$!attr //= value` hands back the attribute

`method table is rw { $!table //= compute() }` (Red's `MetamodelX::Red::Model.table`) returned the
assigned value, so `Bla.^table = "x"` died with "Cannot modify an immutable Str". The tail of an rw
routine that is a compound or plain assignment to a `$!attr` now runs for its effect and returns the
attribute's container, like a bare `$!attr` tail.
