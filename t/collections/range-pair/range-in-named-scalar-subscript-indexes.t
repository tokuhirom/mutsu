use v6;
use Test;

# A Range assigned to a named `$` scalar is itemized, so using that scalar as
# a subscript numifies it (element count => index 3) instead of slicing.
# The bound (`:=`), anonymous (`my $ =`) and `$( )` spellings were already
# correct; the named-scalar `=` store was the one that lost the itemization.

plan 5;

my @n = <4 8 15 16 23 42>;

my $assigned = 1..3;
is @n[$assigned].raku, 'IntStr.new(16, "16")',
    'a Range in a named scalar indexes with its .Numeric (3), not a slice';
is @n[$assigned].elems, 1, '... and yields one element';

is @n[my $ = 1..3].raku, 'IntStr.new(16, "16")', 'anonymous scalar spelling agrees';
is @n[$(1, 2)].raku, 'IntStr.new(15, "15")', '$( ) list spelling agrees';

my $bound := 1..3;
is @n[$bound].raku, '(IntStr.new(8, "8"), IntStr.new(15, "15"), IntStr.new(16, "16"))',
    'a bound (non-itemized) Range still slices';
