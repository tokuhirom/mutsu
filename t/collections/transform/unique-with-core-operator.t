use Test;

# `unique` / `repeated` / `squish` with a core operator as `:with`: `&[===]` is
# the default comparison (taken on the hashed path, as rakudo does), and a
# built-in infix such as `&[==]` is applied directly; both keep their answers
# (found running ParaSeq's t/20-unique, 10000 elements under `:with`, #17337).

plan 7;

is-deeply (1, 1, 2, "1").unique(:with(&[===])).List, (1, 2, "1"), 'unique :with(&[===]) is identity';
is-deeply (1, 1, 2, "1").unique(:with(&[==])).List, (1, 2), 'unique :with(&[==]) compares numerically';
is-deeply (1, 1, 2, 2, 1).squish(:with(&[===])).List, (1, 2, 1), 'squish :with(&[===])';
is-deeply (1, 1, 2, 2, 3).repeated(:with(&[===])).List, (1, 2), 'repeated :with(&[===])';
is-deeply (1.0, 1, 2).unique(:with(&[eq])).List, (1.0, 2), 'unique :with(&[eq]) compares as strings';
my $big = (^3000).List;
is $big.unique(:with(&[===])).elems, 3000, 'a large unique :with(&[===]) completes';
{
    sub infix:<===>($, $) { False }
    is-deeply (1, 1).unique(:with(&[===])).List, (1, 1), 'a user infix:<===> is a comparator';
}
