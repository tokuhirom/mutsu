# `.origin` of a declaration's `shape` (`my Int %h{Str}`, `my @a[3]`)

The `shape` `SemiList` of a key-typed hash or shaped array, its dimension statements and, for a
key type, the `Type::Simple` and `Name` carried no origin. They are now placed from the text
inside the declaration's brackets and checked against it, matching rakudo's ranges (#15780).
