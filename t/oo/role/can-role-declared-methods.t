use Test;

plan 10;

# Rakudo flattens a role's methods into the class that does it, so `.^can`
# finds `Positional.of` / `Associative.keyof` / `QuantHash.keyof` from the
# composing class (#12990). The counts are those of Rakudo.
sub counts(Mu \t) { <of keyof>.map({ t.^can($_).elems }).join(",") }

is counts(List),  '1,0', 'List';
is counts(Array), '2,0', 'Array';
is counts(Hash),  '2,2', 'Hash';
is counts(Range), '1,0', 'Range';
is counts(Pair),  '1,1', 'Pair';
is counts(Map),   '1,1', 'Map';
is counts(Set),   '1,1', 'Set';
is counts(Bag),   '1,1', 'Bag';

# A class that does not compose the role still has no such method.
is Int.^can('of').elems, 0, 'Int has no .of';
is Str.^can('keyof').elems, 0, 'Str has no .keyof';
