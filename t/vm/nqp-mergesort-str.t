use v6;
use nqp;
use Test;

plan 5;

my $a := nqp::list_s("b", "a", "c");
my $s := Rakudo::Sorting.MERGESORT-str($a);
is nqp::elems($s), 3, 'MERGESORT-str keeps every element';
is nqp::shift_s($s), "a", 'the smallest string comes first';
is nqp::shift_s($s), "b", 'then the next';
is nqp::shift_s($s), "c", 'then the largest';

my $b := nqp::list_s("b", "B", "a", "ab", "");
is nqp::join(",", Rakudo::Sorting.MERGESORT-str($b)), ",B,a,ab,b", 'order is by codepoint';
