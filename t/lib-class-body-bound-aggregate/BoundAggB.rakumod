unit class BoundAggB;
# A bound `@`/`%` lexical of the class body, named like BoundAggB's.
my %registry := Hash.new;
my @log := Array.new;
method add(Str $k) { %registry{$k} = 1; @log.push($k) }
method keys-seen { %registry.keys.sort.join(",") }
method log-count { @log.elems }
method id { %registry.WHICH }
