# `.AST` keeps a colonpair argument of a parameterized type

`has NameTree[Dest, :coerce(&f)] $.x` (PDF::Class's `PDF::Names`) made a module fail to load with
"`.AST` does not yet support this construct: type `:coerce`", because a type application that only
survives as source text split its arguments into type spellings and had no case for a colonpair.
A `:name(...)` / `:!name` argument is now parsed as the expression it is and converted like a pair
outside a type, so rakudo's `ColonPair::Value` / `ColonPair::False` nodes appear in the argument list.
