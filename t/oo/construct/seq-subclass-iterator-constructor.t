use v6;
use Test;

plan 14;

# A user subclass of Seq (`unit class Gauge is Seq`) constructs from an
# Iterator like Seq.new, also through `callwith`/`nextwith` from its own `new`,
# and its Seq methods answer on the sequence it carries.
class It does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}

class Plain is Seq { }
my $p = Plain.new(It.new);
isa-ok $p, Plain, 'Plain.new(iterator) builds the subclass';
ok $p ~~ Seq, 'and it is a Seq';
is-deeply Plain.new(It.new).list, (0, 1, 2), '.list reads the carried sequence';
is-deeply Plain.new(It.new).head(2), (0, 1), '.head';
is Plain.new(It.new).iterator.pull-one, 0, '.iterator is the carried iterator';
my @a = Plain.new(It.new);
is-deeply @a, [0, 1, 2], 'list assignment flattens it';
my @seen;
@seen.push($_) for Plain.new(It.new);
is-deeply @seen, [0, 1, 2], '`for` iterates it';
is-deeply Plain.new(It.new).map(* * 2).List, (0, 2, 4), '.map';
my @slipped = |Plain.new(It.new);
is-deeply @slipped, [0, 1, 2], '`|` slips its elements';

class G is Seq {
    method new(::?CLASS:_: Iterator:D $it --> ::?CLASS:D) { callwith $it }
}
isa-ok G.new(It.new), G, 'callwith from the subclass new builds the subclass';
is-deeply G.new(It.new).list, (0, 1, 2), 'and its elements are the iterator\'s';

class H is Seq { method new(Iterator:D $it) { nextwith $it } }
isa-ok H.new(It.new), H, 'nextwith from the subclass new';
is-deeply H.new(It.new).head(2), (0, 1), 'and .head works';
is H.new(It.new).iterator.pull-one, 0, 'and .iterator works';
