use Test;
plan 10;

# Basic hyper-assign on array variable
{
    my @a = 1, 2, 3;
    @a >>+=>> 10;
    is @a.join(","), "11,12,13", '>>+=>> adds to each element';
}

{
    my @a = <a b c>;
    @a >>~=>> "x";
    is @a.join(","), "ax,bx,cx", '>>~=>> concatenates to each element';
}

{
    my @a = 10, 20, 30;
    @a >>-=>> 5;
    is @a.join(","), "5,15,25", '>>-=>> subtracts from each element';
}

{
    my @a = 2, 4, 6;
    @a >>*=>> 3;
    is @a.join(","), "6,12,18", '>>*=>> multiplies each element';
}

# Hyper-assign on array slice
{
    my @a = <a b c d>;
    @a[0, 1] >>~=>> "x";
    is @a.join(","), "ax,bx,c,d", '>>~=>> on slice modifies only sliced elements';
}

{
    my @a = <a b c>;
    @a[0 ..^ *-1] >>~=>> "y";
    is @a.join(","), "ay,by,c", '>>~=>> with WhateverCode slice works';
}

# Verify comparison ops are NOT treated as assign
{
    my @a = 1, 2, 3;
    my @b = 2, 2, 2;
    my @r = @a >>==>> @b;
    is @r.join(","), "False,True,False", '>>==>> is comparison, not assign';
    is @a.join(","), "1,2,3", 'original array unchanged after >>==>>';
}

# flat preserves laziness
{
    is-deeply (42 xx *).flat.is-lazy, True, '(lazy).flat.is-lazy is True';
    is-deeply (42 xx 1).flat.is-lazy, False, '(finite).flat.is-lazy is False';
}
