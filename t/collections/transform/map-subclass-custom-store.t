use v6;
use Test;

# ValueMap (ecosystem): a Map and a Map subclass are immutable. push/append are
# Hash methods, and a positional subscript write must not turn an `is Map`
# instance into a Hash.
plan 8;

class MyMap is Map { }
my %vm is MyMap = a => 1;
my $map = Map.new((a => 1));

for <push append> -> $method {
    dies-ok { $map."$method"() }, "Map.$method does not exist";
    dies-ok { %vm."$method"() }, "Map subclass .$method does not exist";
}

dies-ok { %vm[0] = 42 }, 'positional assignment into a Map subclass dies';
is %vm.^name, 'MyMap', 'the instance is still a MyMap afterwards';
is %vm<a>, 1, 'and its entries are untouched';
dies-ok { $map[0] = 42 }, 'positional assignment into a Map dies';
