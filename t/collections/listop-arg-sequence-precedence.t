use v6;
use Test;

# A list-prefix / unparenthesized-call argument binds the sequence operator
# (`...`/`...^`) TIGHTER than the comma that separates arguments, so
# `none 2 ... 5` is `none(2 ... 5)`, not `(none 2) ... 5`. Previously mutsu
# parsed the sequence OUTSIDE the listop (giving `(none(2) 1 2 3 4 5)`).
# (roast/S02-types/whatever.t "no issues with //= and WhateverCode".)

plan 7;

is (none 2 ... 5).^name, 'Junction', 'none SEQ is a Junction, not a Seq';
is-deeply (none 2 ... 5), none(2, 3, 4, 5), 'none 2 ... 5 == none(2 ... 5)';
ok (7 %% none 2 ... 5), '7 is divisible by none of 2..5';
nok (6 %% none 2 ... 5), '6 IS divisible by one of 2..5';

is (any 1 ... 3).^name, 'Junction', 'any SEQ is a Junction';

# The prime-sieve idiom from the roast test.
{
    my @isprime = False, False;
    my @r = (for 1 .. 10 -> $i {
        $i if @isprime[$i] //= so $i %% none 2 ...^ * > $i.sqrt.floor;
    });
    is-deeply @r, [2, 3, 5, 7], '//= with WhateverCode-terminated none-sequence';
}

# A trailing feed operator still binds OUTSIDE the listop argument.
{
    my @a = 1 .. 4;
    my @b;
    @a ==> grep { $_ %% 2 } ==> @b;
    is-deeply @b, [2, 4], 'feed after a listop-with-block still parses outside the arg';
}
