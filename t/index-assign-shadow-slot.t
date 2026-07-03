use Test;

# §1.4 shadow-slot leaf: element/index assignment on a variable that shadows an
# enclosing same-name binding must write the inner (live) shadow's container, not
# the outer (first) same-name slot. The IndexAssignExprNamed target store now
# prefers the compile-time-baked local slot. Passes with the shadow-slot gate off
# (shared slot) AND on (distinct slots). Mirrors roast/S13-overloading/
# metaoperators.t #14-16. See docs/lexical-scope-slot-campaign.md.

plan 6;

# Scalar auto-vivified to a hash, inside a shadow.
my $a = 1;
{
    my $a;
    $a<x> = 42;
    is $a<x>, 42, 'index-assign auto-vivifies the inner shadow';
}
is $a, 1, 'outer scalar untouched by inner index-assign';

# Hyper index-assign to a shadowed scalar-hash.
my $h = 'outer';
{
    my $h;
    $h<a b c> »=» 7;
    is $h<a>, 7, 'hyper index-assign hits the inner shadow (a)';
    is $h<c>, 7, 'hyper index-assign hits the inner shadow (c)';
}
is $h, 'outer', 'outer scalar untouched by inner hyper index-assign';

# Array element assignment into a shadow.
my @arr = 1, 2, 3;
{
    my @arr = 0, 0, 0;
    @arr[1] = 99;
    is @arr[1], 99, 'array element assign hits the inner shadow';
}
