use Test;

# A WhateverCode subscript (`*-1`, `0..*-2`) is called with the target's
# `.elems`, which a lazy list or an infinite Range cannot report: rakudo dies
# with X::Cannot::Lazy instead of answering from a reified prefix. Every
# expectation below was checked against rakudo (#10781).

plan 10;

throws-like { (1, 2 ... *)[*-1] }, X::Cannot::Lazy, 'an infinite sequence';
throws-like { my @a = 1..*; @a[*-1] }, X::Cannot::Lazy, 'an array assigned an infinite Range';
throws-like { (1..*)[*-1] }, X::Cannot::Lazy, 'an infinite Int Range';
throws-like { ('a'..*)[*-1] }, X::Cannot::Lazy, 'an infinite Str Range';
throws-like { (1..*)[0..*-2] }, X::Cannot::Lazy, 'a WhateverCode yielding a Range';
throws-like { (1, 2 ... *)[0, *-1] }, X::Cannot::Lazy, 'a WhateverCode inside a list index';
throws-like { (1, 2 ... *).lazy[*-1] }, X::Cannot::Lazy, 'a .lazy list';

is (1..10)[*-1], 10, 'a finite Range still resolves *-1';
is (1, 2 ... 10)[*-1], 10, 'a finite sequence still resolves *-1';
is (1..*)[2], 3, 'a plain index into an infinite Range still works';
