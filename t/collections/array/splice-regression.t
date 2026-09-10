use Test;
plan 2;

# splice with offset beyond end should throw X::OutOfRange (per Raku spec)
throws-like { my @a; @a.splice(2, 3) }, X::OutOfRange,
    "splice on empty array beyond end throws X::OutOfRange";

throws-like { my @b = <a b c>; @b.splice(10, 1) }, X::OutOfRange,
    "splice beyond end on populated array throws X::OutOfRange";
