# Chained stores through `is Hash` roots, and `:=` on `is Array` subclasses

`$obj<a><b> = v` and `$obj<a><b> := $x` on an `is Hash` subclass instance
replaced the instance's contents instead of descending into its backing
storage, and `$obj[0] := $x` on an `is Array` subclass created a stray
attribute. Both now work on the storage, as on a plain Hash/Array. PDF 0.6.17
`cos-deref` passes.
