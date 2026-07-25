use Test;

plan 16;

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

# A *nodal* method's result is that node's own value and stays ONE element, even
# though it is a Slip — only a leaf application slips (roast S03-metaops/hyper.t
# "`.Slip` is nodal"). This is the boundary the flattening must not cross.
is [[2, 3], [4, [5, 6]]]».Slip.gist, '((2 3) (4 [5 6]))',
    '.Slip is nodal: each node stays one element';
is [[2, 3], [4, [5, 6]]]».List.gist, '((2 3) (4 [5 6]))',
    '.List is nodal too';

# The callable hyper `>>.&…` is never nodal, so it always slips.
my @n = 1, 2;
is (@n>>.&{ slip($_, $_ * 10) }).raku, '[1, 10, 2, 20]',
    'a callable hyper flattens a Slip too';
is (@n>>.&{ $_ * 2 }).raku, '[2, 4]',
    'and a non-Slip callable result is unaffected';
