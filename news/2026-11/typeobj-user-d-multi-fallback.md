# A type object falls back to the built-in method when a user multi only has `:D` candidates

`class C { multi method Bool(::?CLASS:D:) {...} }; C.Bool` died with "Cannot resolve caller
Bool(C:U: )". As for instances, a call none of the user's candidates accepts now reaches the
built-in method (`Mu.Bool`, False) for a type object too. This was the first of three
BinaryHeap differences (#16864); the whole BinaryHeap suite now matches `raku`.
