# `nextwith` from a user `new` reaches the native ancestor's constructor

`class Reals is Range { method new(...) { nextwith $min, $max, ... } }` died with
"Default constructor ... only takes named arguments": the base candidate of a
`new` deferral on a type object was always `Mu.new`. It is now the core type's
own constructor (`Range.new`, `Pair.new`, ...), as for a subclass without a user
`new` (Math::Sequences `Reals`, #15289).
