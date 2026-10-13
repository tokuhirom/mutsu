use v6;
use Test;

# ADR-11678 §8.4 / #13026 step 3: `.AT-POS` / `.EXISTS-POS` on a plain gather
# pull only the prefix the index needs, as `$g[$i]` does, and leave the Seq
# cached and readable. The method reads the Seq's own stored elements, not
# the gather's inner coroutine list. Every expectation is Rakudo v2026.09's
# output.

plan 16;

{
    my $g = gather { take 1; take 2; take 3 };
    is $g.AT-POS(1), 2, 'AT-POS reads an element';
    nok $g.EXISTS-POS(5), 'EXISTS-POS past the end';
    ok $g.EXISTS-POS(2), 'EXISTS-POS of the last element';
    is $g.List, (1, 2, 3), 'the gather stays readable';
    is $g.List, (1, 2, 3), 'and is cached';
}

{
    my $c = 0;
    my $g = gather { loop { $c++; take $c } };
    is $g.AT-POS(2), 3, 'AT-POS on an infinite gather';
    is $c, 3, 'the body ran only as far as the index';
    ok $g.EXISTS-POS(4), 'EXISTS-POS on an infinite gather';
    is $c, 5, 'the body ran only as far as that index';
    is $g.AT-POS(1), 2, 'an earlier index is served from the cache';
    is $c, 5, 'without running the body again';
}

{
    my \g = gather { take 1; take 2 };
    is g.AT-POS(0), 1, 'a sigilless gather';
    is g.AT-POS(1.0), 2, 'an integral Rat index';
}

{
    my $g = gather { take 1; take 2 };
    is $g.&{ $^a.AT-POS(1) }, 2, 'AT-POS through a block call';
}

{
    my @a = 1, 2;
    my $g = gather { take-rw @a[0] };
    $g.AT-POS(0) = 9;
    is @a, [9, 2], 'an AT-POS element store reaches the take-rw container';
}

is (lazy gather { loop { take 7 } }).AT-POS(3), 7, 'a lazy gather';
