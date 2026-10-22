# HLL conversion of method results

Raku method calls now convert raw NQP return values to their high-level
counterparts, using the same conversion as `nqp::hllize`. A method returning
`nqp::list(...)` yields an iterable `List`; raw hashes and boxed scalars likewise
become `Hash`, `Int`, `Num`, or `Str`. This applies to dynamic method names and
`is raw` methods as well. Subroutine return values retain their raw representation.
Collection conversion continues to share the original storage.
