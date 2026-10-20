# `.^methods` lists the coercers Rakudo declares on Map, Seq, Set, Bag and Mix

Rakudo declares `Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/`MixHash` (and `Map`/`Hash` on `Map`) on those classes themselves. mutsu's rows for them sit on `Any`, which the classes reach through their MRO, so the class's own method table skipped them. `declared_names_for_owner` now accepts a row of any class in the MRO, and `Map.^methods` and its siblings match `raku` for these names.
