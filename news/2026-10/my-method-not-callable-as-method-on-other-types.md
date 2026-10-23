# A lexical `my method` is not a method of every type

`my method foo { ... }` declares a lexical routine `&foo`; it is not entered in any class's
method table, so `Int.foo`, `A.new.foo` and `A.foo` must raise `X::Method::NotFound`. The
instance-method fallback chain ended in "call the lexical `&name` if one exists", which
called it with the call's arguments and without the invocant, so those calls died with
"Too few positionals passed". That fallback is gone; `.&foo` and `foo(...)` still work.

It mattered for Red 0.2.7: `MetamodelX::Red::Specialisable` installs `my method no-table`
on a specialised model, and from then on `Model.?no-table` on every other model reached the
lexical method instead of answering `Nil`, aborting every query's SQL translation.
`Metamodel::ClassHOW.new_type` also treats a name that a declared class already owns as
taken, so a type minted under it gets its own registry entry.

Refs #14094.
