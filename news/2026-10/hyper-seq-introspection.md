# A `HyperSeq` in a variable knows its own type

`my $h = (1..3).hyper; $h.isa(HyperSeq)` answered `False` and `$h.isa(List)` answered `True`,
because method calls on a variable converted the sequence to a `List` before `isa` saw it. `Mu`'s
introspection methods (`WHAT`, `^name`, `isa`, `does`, `defined`, `DEFINITE`, `WHICH`, `WHERE`)
now read the sequence itself on every call path (ADR-11276 §9.173). Other methods still read the
sequence as its `List`, as in Rakudo.
