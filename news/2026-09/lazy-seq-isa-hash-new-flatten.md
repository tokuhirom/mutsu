# Lazy Seq `.isa(Seq)` and `Hash.new` list flattening

`gather`/closure-sequence values now answer `.isa(Seq)` like Rakudo (they were
treated as `Array`), and `Hash.new` flattens nested plain Lists the way its
`*@` slurpy does. Both were found by the Listicles distribution, whose ledger
record goes red -> green.
