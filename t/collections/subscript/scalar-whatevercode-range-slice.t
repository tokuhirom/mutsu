use Test;

# From the Text::Diff distribution: `$line[0 .. *-2]` on a non-list scalar
# slices the one-element list ($line,); an empty range is (), not Nil.
plan 6;

my $s = 'bar';
is $s[0 .. *-2].raku, '()', 'empty WhateverCode range slice is ()';
is $s[0 .. *-1].raku, '("bar",)', 'full WhateverCode range slice';
is $s[0 ..^ *-1].raku, '()', 'exclusive end, empty';
is $s[0 ..^ *].raku, '("bar",)', 'exclusive end, full';
is "+--".sprintf($s[0 .. *-2]), '+--', 'empty slice flattens to no sprintf args';
dies-ok { $s[1 .. *] }, 'range starting past index 0 is out of range';
