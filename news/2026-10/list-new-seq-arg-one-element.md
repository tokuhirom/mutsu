# `List.new(Seq)` keeps the Seq as one element

`List.new((1,2).Seq)`, `List.new(gather {...})` and `List.new(1..3)` flattened their
argument. `List.new` is a `**@` slurpy, so each argument stays one element; only
`Array.new` and the typed-array constructors spread a single Positional (#13071).
