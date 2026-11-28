use Test;

# #18267 / ADR-19315: a slot-frame method whose body is closed arithmetic
# over its parameters, its attributes and its parameters' accessors runs as a
# method kernel. Whatever the kernel cannot compute exactly must come out as
# the ordinary call computes it.

plan 16;

class P {
    has $.x;
    has $.y;
    method dist(P $o) { (($!x - $o.x) ** 2 + ($!y - $o.y) ** 2).sqrt }
    method lin($k) { $!x * $k + $!y }
    method sq() { return $.x * $.x }
}

my $a = P.new(x => 0, y => 0);
my $b = P.new(x => 3, y => 4);
my $sum = 0e0;
$sum += $a.dist($b) for ^300;
is $sum, 1500e0, 'a hot integer distance';
isa-ok $a.dist($b), Num, 'sqrt answers a Num';

is P.new(x => 2, y => 1).lin(10), 21, 'Int arithmetic stays Int';
isa-ok P.new(x => 2, y => 1).lin(10), Int, 'and is an Int';
is P.new(x => 2, y => 1).lin(0.5), 2.0, 'a Rat argument (the generic path)';
isa-ok P.new(x => 2, y => 1).lin(0.5), Rat, 'keeps its Rat type';
is P.new(x => 1.5e0, y => 1).lin(2), 4e0, 'a Num attribute mixes into a Num';

is P.new(x => 2**62, y => 0).lin(4), 2**64, 'an overflow widens to a big Int';
is P.new(x => 7, y => 0).sq, 49, 'self accessors';
is P.new(x => '3', y => 0).sq, 9, 'a Str attribute (the generic path)';

{
    my $p = P.new(x => 1, y => 1);
    my @r;
    for ^20 { @r.push: $p.lin(1) }
    is @r.sum, 40, 'a warm kernel';
    my $q = P.new(x => 10, y => 1);
    is $q.lin(1), 11, 'another receiver of the same class';
}

is P.new(x => -4, y => 0).dist(P.new(x => 0, y => 3)), 5e0, 'negative coordinates';

class Q is P { }
is P.new(x => 0, y => 0).dist(Q.new(x => 6, y => 8)), 10e0, 'a subclass argument';

{
    my @t = await (^4).map: -> $t {
        start { my $s = 0e0; $s += $a.dist($b) for ^200; $s }
    };
    is-deeply @t, [1000e0 xx 4], 'concurrent kernel calls';
}

{
    multi sub infix:<->(Int:D $l, 1) { 100 }
    class R { has $.v = 5; method m($w) { $!v - $w } }
    is R.new.m(1), 100, 'a user infix declared in scope wins';
}
