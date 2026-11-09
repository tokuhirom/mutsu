use v6;
use Test;

# A module-level Map that one thread republishes (under a Lock) while others
# bind `my \hit = $cache{key}` lock-free. The bind path used to clone the
# shared cell's value under the cell mutex only, taking a refcount on a node
# the writer was dropping (#17149); it now reads through the stripe.
plan 4;

my $cache = Map.new;
my $lock = Lock.new;
my $bad = 0;

my @readers = (^3).map: -> $t {
    start {
        for ^4000 -> $i {
            my \hit = $cache{"k" ~ ($i % 8)};
            $bad++ unless hit.defined ?? hit ~~ Int !! True;
        }
    }
};
my $writer = start {
    for ^4000 -> $i {
        $lock.protect: {
            my %n = $cache.pairs;
            %n{"k" ~ ($i % 8)} = $i;
            $cache = Map.new(%n.pairs);
        }
    }
};
await $writer, |@readers;
is $bad, 0, 'lock-free sigilless binds of a republished Map never see a bad value';
ok $cache.elems <= 8, 'the final Map is intact';

# Assigning a new Map to a variable never rewrites the old Map in place.
my $a = Map.new('a', 1);
my $b = $a;
$a = Map.new('b', 2);
is-deeply $b.keys.sort.List, ('a',), 'an alias of the old Map keeps its contents';
is-deeply $a.keys.sort.List, ('b',), 'the variable holds the new Map';
