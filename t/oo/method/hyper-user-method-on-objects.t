use Test;

# A hyper call of a user method over objects dispatches each element as a
# value receiver (#9494); attribute writes still land on the objects.

plan 9;

class C {
    has $.v is rw;
    method Str { "c$!v" }
    method bump { $!v++; self }
    method set($x) { $!v = $x; $x * 2 }
}
my @a = (1..3).map({ C.new(v => $_) });
is-deeply @a».Str, ['c1', 'c2', 'c3'], 'a user Str per element';
is-deeply @a».bump».v, [2, 3, 4], 'a mutating method, chained';
is-deeply @a».v, [2, 3, 4], 'the mutation is on the objects';
is-deeply @a».set(10), [20, 20, 20], 'with an argument';
is-deeply @a».v, [10, 10, 10], 'and its attribute write';

my $x = C.new(v => 7);
my @b = $x, $x;
@b».bump;
is $x.v, 9, 'one object twice is bumped twice';

is (C.new(v => 1), (C.new(v => 2), C.new(v => 3)))».Str.raku,
    '("c1", $("c2", "c3"))', 'nested lists descend';

class D is C { method Str { "d" ~ callsame } }
is (D.new(v => 4), C.new(v => 5))».Str.raku, '("dc4", "c5")',
    'each element dispatches on its own class';

class E { method boom { die "boom" } }
throws-like { (E.new,)».boom }, Exception, message => 'boom',
    'an exception propagates';
