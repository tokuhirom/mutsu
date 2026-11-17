# `set(1,2) ~~ 2` and `(1,2).Seq ~~ 2` are True

`Numeric.ACCEPTS` compares `topic == self`, so an aggregate topic numifies
through its own `.Numeric`. Smartmatching a Set, a Hash or a Seq against a
number now compares its element count, and a Bag or Mix compares its total
weight, as on rakudo. Before, only an Array or a Buf did. The comparison uses
`coerce_to_numeric`, the same numification the numeric operators use. A Seq
whose elements have not been pulled yet is reified first and stays readable.
`Range.ACCEPTS` compares a Seq's or Hash's element count the same way, so
`(1,2).Seq ~~ 1..3` is True.
