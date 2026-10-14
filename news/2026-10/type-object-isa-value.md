# A value argument to isa on a type object stands for its type

`Int.isa(3)` answered False because the interpreter's `isa` arms compared the
argument's string form. Rakudo's `Mu.isa(Mu \type)` reads the argument's type,
so the value `3` stands for `Int`. The type-object and instance arms now use
the value's type name, as the `Mu.isa` method-table row does (#13260).
