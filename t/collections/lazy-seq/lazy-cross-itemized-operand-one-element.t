use Test;

# An itemized operand of a lazy `X` (one operand is infinite) is one element of
# its column, as it is on the finite path, not its items (#15082). Checked
# against rakudo.

plan 6;

my @b = 1, 2, 3;

is-deeply (@b.item X (1..*)).head(2), (($[1, 2, 3], 1), ($[1, 2, 3], 2)).Seq,
    '@b.item X (1..*): the itemized array is one element';
is-deeply ($(1, 2, 3) X (1..*)).head(2), (($(1, 2, 3), 1), ($(1, 2, 3), 2)).Seq,
    'a $(...) itemized list is one element too';
is-deeply ((1, 2, 3).item X (1..*)).head(2), (($(1, 2, 3), 1), ($(1, 2, 3), 2)).Seq,
    'so is (...).item';
is-deeply ((1, 2) X (3, 4)), ((1, 3), (1, 4), (2, 3), (2, 4)).Seq,
    'a plain finite product is unchanged';
is-deeply ((1, 2, 3) X (1..*)).head(2), ((1, 1), (1, 2)).Seq,
    'a non-itemized list still contributes its items';
is-deeply (@b.item X (1, 2)), (($[1, 2, 3], 1), ($[1, 2, 3], 2)).Seq,
    'the finite path agrees with the lazy one';
