use Test;

# A pointy block's capture parameter takes a sub-signature that unpacks the
# captured arguments (annotations' `Keeper`, #15259).

plan 6;

my $f = -> | ($, $v) { $v };
is $f(1, 2), 2, 'an anonymous capture with a sub-signature';

my $g = -> |c ($a, $b) { "$a-$b" };
is $g(3, 4), '3-4', 'a named capture with a sub-signature';

my $h = -> | { 5 };
is $h(1, 2), 5, 'an anonymous capture without one still works';

my $i = -> |c { c.elems };
is $i(1, 2, 3), 3, 'a named capture without one still works';

my $j = -> |c ($a, :$n) { "$a/$n" };
is $j(1, :n(7)), '1/7', 'the sub-signature can take named parameters';

throws-like { -> |c ($a, $b) { 1 }.(1) }, X::AdHoc, 'the sub-signature is enforced';
