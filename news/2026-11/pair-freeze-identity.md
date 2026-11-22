# A Pair holding a container has a stable identity, and `.freeze` keeps it

A Pair whose value is a container (`my $v; my $p = a => $v`) is identified
by the Pair object, not by its content: rakudo gives it an `ObjAt`. mutsu
sent it to the global-counter fallback of `.WHICH`, so every read gave a
different id. `Pair.freeze` also built a new Pair and rebound the variable by
name, which changed the identity again and missed any other holder of the
same Pair.

`PairData` now carries a `WhichId`, the Pair object's identity. It is minted
on first use and is fresh for a `.clone`. `Value::freeze_pair` replaces the
value inside the shared Pair object with a read-only container, as rakudo's
`freeze` binds `$!value` of the same object. The method-table row and both VM
intercepts use it. roast `6.c/APPENDICES/A03-older-specs/01-misc.t` now passes
its `Pair.freeze` group.
