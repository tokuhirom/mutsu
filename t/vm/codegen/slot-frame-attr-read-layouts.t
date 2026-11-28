use Test;

# #18267: a slot-frame method reads `$!x` straight off its receiver's layout
# slot, and a warm accessor site reads `$o.x` from the slot it remembers.
# Both must notice a receiver of another layout, a write, a rebinding and a
# subclass that moves the attribute.

plan 12;

class P {
    has $.x;
    has $.y;
    method sum() { $!x + $!y }
    method xx() { $!x * $!x }
}
class Q is P {
    has $.z = 100;
    method zsum() { $!z + 1 }
}

sub call-sum($o) { $o.sum }
sub read-x($o) { $o.x }

my @r;
@r.push: call-sum(P.new(x => $_, y => 1)) for ^5;
is-deeply @r, [1, 2, 3, 4, 5], 'a warm slot-frame attribute read';
is call-sum(Q.new(x => 10, y => 20)), 30, 'a subclass instance (another layout)';
is call-sum(P.new(x => 7, y => 7)), 14, 'and back to the base layout';
is Q.new(x => 1, y => 2).zsum, 101, 'the subclass reads its own attribute';

{
    my $p = P.new(x => 3, y => 4);
    is $p.xx, 9, 'before a write';
    $p.x = 5 if $p.^attributes.first(*.name eq '$!x').rw;
    my $q = P.new(x => 6, y => 0);
    is $q.xx, 36, 'another instance';
}

{
    class R { has $.v is rw; method twice() { $!v * 2 } }
    my $r = R.new(v => 2);
    is $r.twice, 4, 'an rw attribute read';
    $r.v = 21;
    is $r.twice, 42, 'the read sees a write through the accessor';
}

@r = ();
@r.push: read-x(P.new(x => $_, y => 0)) for ^5;
is-deeply @r, [0, 1, 2, 3, 4], 'a warm accessor site';
is read-x(Q.new(x => 'q', y => 0)), 'q', 'the accessor site on a subclass layout';

{
    class S { has @.list; has $.n = 1; method n2() { $!n + 1 } }
    my $s = S.new(list => [1, 2, 3]);
    is-deeply $s.list, [1, 2, 3], 'an aggregate accessor read';
    is $s.n2, 2, 'a scalar attribute next to an aggregate one';
}
