use Test;

# `@a[$i] = $v` with $i exactly one past the end appends (the idiom for building
# an array element by element); the compiled element-store lane serves it. A
# store that leaves a gap, a typed array, an `is default` array and a bound
# array keep their own rules. Checked against rakudo.
plan 13;

{
    my @a;
    @a[$_] = $_ * 2 for ^5;
    is @a.join(','), '0,2,4,6,8', 'appending by index builds the array';
    is @a.elems, 5, '... with the right element count';
    ok @a[4]:exists, '... and every element exists';
}

{
    my @a = 1, 2, 3;
    @a[3] = 4;
    @a[4] = 5;
    is @a.join(','), '1,2,3,4,5', 'appending to a populated array';
}

{
    my @a;
    @a[0] = 1;
    @a[3] = 4;
    is @a.elems, 4, 'a gap store still sizes the array by the subscript';
    nok @a[1]:exists, '... and the gap is a hole';
    ok @a[3]:exists, '... while the stored element exists';
    @a[4] = 5;
    nok @a[2]:exists, 'an append after a gap leaves the hole alone';
}

{
    my Int @t;
    @t[0] = 1;
    @t[1] = 2;
    is @t.join(','), '1,2', 'appending to a typed array works';
    throws-like { @t[2] = "x" }, X::TypeCheck::Assignment, 'an append to a typed array is still checked';
}

{
    my @d is default(7);
    @d[0] = 1;
    @d[2] = 3;
    is @d[1], 7, 'an is default array keeps its default for a gap';
}

{
    my @src;
    my @alias := @src;
    @alias[0] = 'a';
    @alias[1] = 'b';
    is @src.join(','), 'a,b', 'an append through a := alias is seen by the source';
}

sub build(Int $n) {
    my @r;
    for ^$n -> $i { @r[$i] = $i }
    @r
}
is build(4).join(','), '0,1,2,3', 'appending inside a sub, from a loop variable';
