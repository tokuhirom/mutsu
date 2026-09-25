use Test;

# A user-declared infix spelled with hyper markers (OneSeq's `infix:«>>>»` /
# `infix:«<<<»`) must win over a hyper-operator reading by longest-token
# matching, including in a chain: `@a >>> @b >>> @c` used to fail to parse
# because the second `>>>` was read as a hyper `>>` opener.

plan 7;

my sub infix:«>>>»(**@its) is assoc<list> is equiv(&[~]) { @its.map(*.Slip).List }
my sub infix:«<<<»(**@its) is assoc<list> is equiv(&[~]) { @its.reverse.map(*.Slip).List }

my @a = 1, 2;
is-deeply (@a >>> @a), (1, 2, 1, 2), 'single >>>';
is-deeply (@a >>> @a >>> @a), (1, 2, 1, 2, 1, 2), 'chained >>> (list associative)';
my @e = @a >>> (3,) >>> @a;
is-deeply @e, [1, 2, 3, 1, 2], 'chained >>> as an assignment RHS';
is-deeply ([>>>] (1, 2), (3,)), (1, 2, 3), 'reduction [>>>]';
is-deeply (@a <<< (3,)), (3, 1, 2), 'the <<< spelling';
is-deeply ((1, 2, 3) >>+<< (10, 20, 30)), (11, 22, 33), 'a real hyper op still parses';
is-deeply ((1, 2) »~» 'x'), ('1x', '2x'), 'a unicode hyper op still parses';
