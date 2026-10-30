# A colonpair argument of a parameterized type survives the RakuAST round trip

`has PDF::NameTree[DestNamed, :coerce(&coerce-dest)] $.Dests is entry;` (PDF::Names, a
PDF::Class module) failed to load with "`.AST` does not yet support this construct: type
`:coerce`". A type application whose arguments come from the source spelling now parses a
non-type argument such as `:coerce(&f)` as an expression instead of rejecting it, so the
declaration lowers like any other pair argument.
