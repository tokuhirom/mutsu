use Test;
plan 4;

my @a = (1..5).list;
is @a.elems, 5, "range list elems";
is @a[0], 1, "range list first";
is @a[4], 5, "range list last";

my @b = (0..^3).list;
is @b.elems, 3, "exclusive range list elems";
