use Test;

# The subset/superset operators are declared over `Any`, so a Junction operand
# autothreads (all/none before any/one) instead of being coerced into a
# one-element Set. From Tinky::JSON's t/020-construction.t:
#   $w2.states.map(*.WHICH).none ⊆ $w1.states.map(*.WHICH).any

plan 9;

is-deeply (1 ⊆ any(1, 4)), any(True, False), '⊆ threads a Junction on the right';
is-deeply (any(1, 4) ⊆ 1), any(True, False), '⊆ threads a Junction on the left';
is-deeply (1 (<=) any(1, 4)), any(True, False), '(<=) is the same operator';
is-deeply (any(1, 4) ⊇ 1), any(True, False), '⊇ threads';
is-deeply (1 ⊂ any(1, 4)), any(False, False), '⊂ threads';
is-deeply (set(1, 2) ⊃ all(1, 2)), all(True, True), '⊃ threads';

ok so(none(1, 2) ⊆ any(3, 4)), 'none(...) ⊆ any(...): no element is in the other list';
nok so(none(1, 2) ⊆ any(1, 4)), 'none(...) ⊆ any(...): one element is shared';

class P { }
my @a = P.new xx 2;
my @b = P.new xx 2;
ok so(@b.map(*.WHICH).none ⊆ @a.map(*.WHICH).any), 'distinct objects share no identity';
