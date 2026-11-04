use Test;
use NativeCall;

plan 6;

# A WhateverCode subscript on a CArray goes through `Code.POSITIONS(array)`,
# the code called with the array's element count (#16488).
my $c = CArray[num64].new;
$c[$_] = $_ * 1.5e0 for ^5;

is $c[*-1], 6, '*-1 is the last element';
is $c[*-2], 4.5, '*-2';
is-deeply $c[*-3 .. *-1].list, (3e0, 4.5e0, 6e0), 'a range of WhateverCode endpoints';
is-deeply $c[*].list, (0e0, 1.5e0, 3e0, 4.5e0, 6e0), 'the whole array';

my $i = CArray[int32].new(10, 20, 30);
is $i[*-1], 30, 'an int32 CArray';
my $w = *-1;
is $w.POSITIONS(<a b c>), 2, 'WhateverCode.POSITIONS calls the code with .elems';
