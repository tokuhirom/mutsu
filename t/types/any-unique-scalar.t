use Test;

plan 8;

# Any.unique is self.list.unique: a scalar receiver answers a one-element Seq.
is 1.unique.raku, '(1,).Seq', 'Int.unique is a one-element Seq';
is "a".unique.raku, '("a",).Seq', 'Str.unique is a one-element Seq';
is <1>.unique.raku, '(IntStr.new(1, "1"),).Seq', 'allomorph.unique is a one-element Seq';
isa-ok 1.5.unique, Seq, 'Rat.unique is a Seq';
is 1.unique.elems, 1, 'one element';
is "a".unique(:as({ .uc })).raku, '("a",).Seq', ':as on a scalar';
is (1, 1, 2).unique.raku, '(1, 2).Seq', 'List.unique unchanged';
is <a b a>.unique.join, 'ab', 'a list of allomorph-free strings unchanged';

# vim: expandtab shiftwidth=4
