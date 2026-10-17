# The constructors are late rows

`new`, `bless`, `new-from-pairs`, `Range.new`, `IO::ArgFiles.new` and `Failure.handled` were the last by-name match of
the method cascade. They are now late rows of the method table, found by the receiver's MRO (ADR-11276 §9.141).

`new-from-pairs` belongs to the QuantHashes, as in Rakudo, so `Int.new-from-pairs(1)` is now "No such method".
