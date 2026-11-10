use Test;

# The set relations are iffy chaining operators: looser than `..`, tighter
# than `&&`. Every line is identical under rakudo.
plan 12;

is-deeply (1..2 ∈ [1..2]), False, '`..` binds tighter than ∈';
is-deeply (1..2 ∈ (1..2,)), True, 'the Range itself is the element';
is-deeply (1..2 (elem) (1..2,)), True, 'ASCII (elem) has the same level';
is-deeply (2 ∈ 1..3), True, 'an element of a range on the right';
is-deeply (1..3 ∋ 2), True, 'a range on the left of ∋';
is-deeply ((1, 2) ⊆ (1, 2, 3)), True, 'a subset of a list';
is-deeply (1..2 ∌ 5), True, 'negated glyph at the chaining level';
is-deeply (3 !(elem) 1..2), True, 'negated meta form with a range right';
my @a = 1, 2;
is-deeply (1 ∈ @a == True), False, 'relations chain with comparisons';
is-deeply (1 ∈ @a && 2 ∈ @a), True, 'looser than the relations is &&';
is-deeply (1 + 1 ∈ @a), True, 'tighter than the relations is +';
is-deeply (1 | 5 ∈ @a), True, 'junctive | is tighter than ∈';
