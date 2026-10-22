use Test;

# #14588 (ValueClass t/01-basic.rakutest): a Set/Bag subscript keys an object
# by its user-defined `.WHICH`, as `set(...)` and `∈` already do.

plan 6;

class P {
    has $.a;
    method WHICH { ValueObjAt.new("P|" ~ $!a) }
}

my $set = set(P.new(:3a) xx 3);
is $set.elems, 1, 'equal WHICH collapse in set()';
ok $set{P.new(:3a)}, 'Set subscript finds an equal object';
nok $set{P.new(:4a)}, 'and not a different one';

my $bag = bag(P.new(:3a) xx 4);
is $bag{P.new(:3a)}, 4, 'Bag subscript counts by WHICH';
is $bag{P.new(:9a)}, 0, 'absent key is 0';

my %h{Any}; %h{P.new(:1a)} = 'x';
is %h{P.new(:1a)}, 'x', 'object hash (unchanged)';
