use Test;

# `.sort` is answered by the method table's `sort` rows for every receiver
# shape (ADR-11276); the VM's former name-matching interceptor is gone. The
# comparator, the mapper and a key method are called the same way for each
# shape: a compiled block, a WhateverCode and a routine all bind, and a
# container-wrapped element is keyed by its value.

plan 14;

my @a = 3, 1, 2;
is-deeply @a.sort, (1, 2, 3), 'an Array';
is-deeply @a.sort({ $^b <=> $^a }), (3, 2, 1), 'a two-argument block comparator';
is-deeply @a.sort(-*), (3, 2, 1), 'a WhateverCode mapper';
is-deeply @a.sort(&infix:«<=>»), (1, 2, 3), 'a routine comparator';
is-deeply @a.sort(:k), (1, 2, 0), ':k answers the indices';

my %h = b => 2, a => 1, c => 0;
is-deeply %h.sort(*.value).map(*.key).List, <c a b>, 'a Hash by a mapper';
is-deeply (1..5).sort(-*), (5, 4, 3, 2, 1), 'a Range';
is-deeply <b a c>.Seq.sort, <a b c>, 'a Seq';
is-deeply bag(<a b a>).sort(-*.value).map(*.key).List, <a b>, 'a Bag by a mapper';
is-deeply set(3, 1, 2).sort.map(*.key).List, (1, 2, 3), 'a Set';

my @n = [1, 2, 3], [1], [1, 2];
my @outer;
@outer.push: $_ for @n;
is-deeply @outer.sort(*.elems).map(*.elems).List, (1, 2, 3),
    'itemized element arrays are keyed by their own elems';
is-deeply @outer.sort({ .elems }).map(*.elems).List, (1, 2, 3), '... through a block too';

my class P { has $.v; method key { $!v } }
is-deeply (P.new(:v(2)), P.new(:v(1))).sort(*.key).map(*.v).List, (1, 2),
    'a user method as the key';
is-deeply (P.new(:v(2)), P.new(:v(1))).sort({ $^a.key <=> $^b.key }).map(*.v).List, (1, 2),
    'a block comparator calling a user method';
