# Qualified calls through nested roles, and slice adverbs on `is Role` hashes

`self.A::m()` now resolves when `A` is only reachable through another role
(`role B does A`), for class instances, punned roles, mixins and type objects.
The `:k`/`:kv`/`:p`/`:v` subscript adverbs now work on a Hash that had an
Associative role mixed in with `my %m is Role`. Found via the Map::Ordered
distribution, whose `t/01-basic.rakutest` now passes 18/18.
