use Test;
plan 8;

# A string range ends once the successor is longer than the end: it must not run
# on through the longer strings that still sort before the end.
is ("a".."zz").elems, 702, "a..zz has 26 + 26*26 elements";
is-deeply ("a".."zz").tail(3).list, ("zx", "zy", "zz"), "the last elements of a..zz";
is ("A".."ZZ").elems, 702, "upper case";
is ("aa".."zz").elems, 676, "equal-length ends";
is ("a"..^"zz").elems, 701, "excluded end";
is-deeply ("aa".."b").list, ("aa",), "start longer than the end";
is ("x".."ab").elems, 0, "start sorts after the end";
my $n = 0;
$n++ for "a".."zz";
is $n, 702, "a for loop over a..zz";
