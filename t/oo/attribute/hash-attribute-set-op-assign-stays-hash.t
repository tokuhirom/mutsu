use Test;

# Found via RedFactory/Red: `%!relationships ∪= $attr` (Red::Relationship) left a
# plain `has %!h` attribute holding a Set, so `.keys.map: { .name ... }` later saw
# the wrong thing. A plain hash attribute assigned a Set stays a Hash; only an
# attribute declared `is SetHash` / `is BagHash` keeps its QuantHash type.

plan 7;

role RR {
    has %!h;
    has %!t{Mu};
    method add($k) { %!h ∪= $k; %!t ∪= $k }
    method h { %!h }
    method t { %!t }
}
class C does RR { }

class CC {
    has %!h;
    has %!t{Mu};
    method add($k) { %!h ∪= $k; %!t ∪= $k }
    method h { %!h }
    method t { %!t }
}

for C, CC -> $T {
    my $c = $T.new;
    $c.add("a");
    $c.add("b");
    is $c.h.WHAT.raku, 'Hash', "{$T.^name}: %!h ∪= x leaves a Hash";
    is-deeply $c.h.keys.sort.List, ("a", "b"), "{$T.^name}: and keeps both elements";
    is $c.t.WHAT.raku, 'Hash[Any,Mu]', "{$T.^name}: an object-hash attribute keeps its type";
}

class Q {
    has %!s is SetHash;
    has %!b is BagHash;
    method add($x) { %!s ∪= $x; %!b ⊎= $x; self }
    method s { %!s }
    method b { %!b }
}
my $q = Q.new.add("a").add("b");
is $q.s.WHAT.raku ~ ' ' ~ $q.b.WHAT.raku, 'SetHash BagHash', 'is SetHash / is BagHash attributes keep their type';
