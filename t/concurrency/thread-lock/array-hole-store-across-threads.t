use v6;
use Test;

plan 9;

# An `@a[$i] = $v` past the end made through the cross-thread lane (inside a
# `start` block, or on the main thread once a thread has run) grew the array
# with `Nil`s instead of the `Any` holes `ArrayData::store_element` grows: the
# gaps printed as `Nil` and claimed to exist.

{
    my @a;
    await start { @a[1] = 1 };
    is @a.gist, '[(Any) 1]', 'a gap grown in a start block is an Any hole';
    nok @a[0]:exists, 'the gap does not exist';
    ok @a[1]:exists, 'the assigned slot exists';
}

{
    my @*A;
    await start { @*A[1] = 1 };
    is @*A.gist, '[(Any) 1]', 'same for a dynamic array';
}

{
    my @a = 1, 2;
    await start { @a[3] = 9 };
    is @a.raku, '[1, 2, Any, 9]', 'growing a populated array keeps its elements';
    ok @a[1]:exists, 'an element present before the store still exists';
    nok @a[2]:exists, 'the new gap does not exist';
}

{
    my @a is default(7);
    await start { @a[1] = 1 };
    is @a.gist, '[7 1]', 'a gap of an `is default` array reads as the default';
}

{
    my @a;
    await start { @a[1] = 1; @a[3] = 3 };
    is @a.raku, '[Any, 1, Any, 3]', 'two stores in one block both keep their gaps';
}
