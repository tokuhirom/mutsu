use v6;
use Test;

# A module-level Map that one thread republishes (under a Lock) while others
# bind `my \hit = $cache{key}` lock-free. The bind path used to clone the
# shared cell's value under the cell mutex only, taking a refcount on a node
# the writer was dropping (#17149); it now reads through the stripe.
plan 2;

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
