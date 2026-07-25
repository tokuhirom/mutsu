use Test;

plan 12;

# A hyper is built on `deepmap`, so a method that returns a `Slip` contributes
# its *elements* to the result, exactly as it would from `map`.

class C {
    has $.n;
    method made()  { slip('a' ~ $!n, { k => $!n }) }
    method plain() { slip('p' ~ $!n, 'q' ~ $!n) }
    method empty() { Empty }
    method one()   { slip('s' ~ $!n) }
    method flat-v() { 'v' ~ $!n }
}

my @objs = C.new(n => 1), C.new(n => 2);

is @objs>>.plain.elems, 4, 'a Slip of two plain values flattens into the result';
is @objs>>.plain.raku, '["p1", "q1", "p2", "q2"]', 'and in order';

is @objs>>.made.elems, 4, 'a Slip containing a Hash flattens too';
ok @objs>>.made[1] ~~ Associative, 'the Hash stays a Hash, not decomposed into Pairs';
is @objs>>.made[1]<k>, 1, 'and keeps its contents';
ok @objs>>.made[3] ~~ Associative, 'second element likewise';

is @objs>>.empty.elems, 0, 'an empty Slip contributes nothing';
is @objs>>.one.elems, 2, 'a one-element Slip contributes one each';
is @objs>>.flat-v.raku, '["v1", "v2"]', 'a non-Slip result is unaffected';

# `.map` already behaved this way; the hyper must agree with it.
is @objs>>.plain.raku, @objs.map(*.plain).List.Array.raku,
    'the hyper agrees with .map on Slip results';

# The same through a nested source: descending itemizes, so the inner result is
# NOT slipped into the outer list.
my @nested = [C.new(n => 3),], [C.new(n => 4),];
is @nested>>.flat-v.elems, 2, 'descending keeps one result per source element';
is @nested>>.flat-v.raku, '[["v3"], ["v4"]]', 'and keeps the nesting';
