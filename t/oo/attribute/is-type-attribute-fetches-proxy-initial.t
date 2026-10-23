use Test;

# An `is Type` attribute stores the value a caller hands it, not the `Proxy`
# standing for it: ValueClass's `clone` passes `.get_value` of attributes it
# holds behind a `Proxy`, and `has @.c is Tuple` built `Tuple.new(Tuple.new(...))`.

plan 5;

class C {
    has @.l is List;
    has %.b is BagHash;
}

my $list = (1, 2, 3);
my $pl := Proxy.new(FETCH => sub (|) { $list }, STORE => sub (|) { die "no" });
is C.new(:l($pl)).l.raku, '(1, 2, 3)', 'a Proxy for a List fills an is-List attribute';
is C.bless(:l($pl)).l.elems, 3, '... through bless too';

my $bag = (a => 2, b => 1).Bag;
my $pb := Proxy.new(FETCH => sub (|) { $bag }, STORE => sub (|) { die "no" });
is-deeply C.new(:b($pb)).b.Bag, $bag, 'a Proxy for a Bag fills an is-BagHash attribute';

my %args = l => $pl;
is C.new(|%args).l.elems, 3, 'a Proxy slurped from a hash fills the attribute';
is C.new(:l($list)).l.elems, 3, 'a plain value still works';
