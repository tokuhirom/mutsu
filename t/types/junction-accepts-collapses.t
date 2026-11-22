use Test;

# `Junction.ACCEPTS` is Junction's own method (ADR-11276): it collapses the
# per-value ACCEPTS by the junction's kind into one Bool, and a junction topic
# is matched as a whole. It agrees with `topic ~~ junction`.

plan 12;

is-deeply (1|2).ACCEPTS(2), True, 'any: one value accepts';
is-deeply (1|2).ACCEPTS(3), False, 'any: no value accepts';
is-deeply (1&2).ACCEPTS(1), False, 'all: not every value accepts';
is-deeply (1&1).ACCEPTS(1), True, 'all: every value accepts';
is-deeply none(1, 2).ACCEPTS(3), True, 'none';
is-deeply one(1, 1).ACCEPTS(1), False, 'one: two values accept';
is-deeply one(1, 2).ACCEPTS(1), True, 'one: exactly one value accepts';
is-deeply (1|2).ACCEPTS(1|5), True, 'a junction topic is matched as a whole';
is-deeply any(Int, Str).ACCEPTS(5), True, 'type objects as values';
is-deeply (1|2).ACCEPTS(Int), False, 'a type object topic';

my $j = 1|2;
is-deeply $j.ACCEPTS(2), (2 ~~ $j), '.ACCEPTS agrees with ~~';
ok $j.ACCEPTS(2) ~~ Bool, 'the answer is a Bool, not a junction';
