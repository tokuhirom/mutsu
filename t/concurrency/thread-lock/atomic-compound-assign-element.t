use Test;

# `⚛+=` / `⚛-=` on an array or hash ELEMENT (#12005). A variable target already
# parsed; an element target was a parse error. The update is the element's own
# atomic add (`atomic-add-fetch(@a[0], 5)`, the delta negated for `⚛-=`), and a
# non-native target is refused under the operator the program wrote, as for
# `@a[0]⚛++` (#11834). Every expected answer is Rakudo 2026.09's.

plan 19;

# ---- native-int arrays: the update works, as a statement and as an expression

{
    my atomicint @a = 0, 0;
    @a[0] ⚛+= 5;
    @a[1] ⚛-= 2;
    is-deeply @a, array[atomicint].new(5, -2), '⚛+= / ⚛-= as statements update the elements';

    is (@a[0] ⚛+= 3), 8, '⚛+= answers the new value';
    is (@a[1] ⚛-= 1), -3, '⚛-= answers the new value';
    is-deeply @a, array[atomicint].new(8, -3), '... and stored it';

    my $i = 1;
    @a[$i] ⚛+= 10;
    is @a[1], 7, 'a variable index';
    @a[$i++] ⚛+= 1;
    is $i, 2, 'the index expression is evaluated once';
    is @a[1], 8, '... and the update lands on it';
}

{
    my int @c = 1, 2;
    is (@c[0] ⚛-= 4), -3, '⚛-= on an int array';
    is-deeply @c, array[int].new(-3, 2), '... the other element is untouched';
    @c[1] ⚛+= 40;
    is @c[1], 42, '⚛+= on an int array';
}

# The operand is an item: `⚛+=` takes the comma list apart like `+=`.
{
    my atomicint @a = 1, 1;
    my @seen = (@a[0] ⚛+= 2, 9);
    is-deeply @seen, [3, 9], 'the right-hand side stops before the comma';
}

# ---- a non-native target is refused, naming the operator ------------------

{
    my @plain = 0;
    my %hash = k => 1;
    throws-like { @plain[0] ⚛+= 5 }, X::Multi::NoMatch,
        message => /'infix:<⚛+=>(Int:D, Int:D)'/,
        '⚛+= on a plain array element';
    throws-like { @plain[0] ⚛-= 5 }, X::Multi::NoMatch,
        message => /'infix:<⚛-=>(Int:D, Int:D)'/,
        '⚛-= on a plain array element';
    throws-like { %hash<k> ⚛+= 1 }, X::Multi::NoMatch,
        message => /'infix:<⚛+=>(Int:D, Int:D)'/,
        '⚛+= on a hash element';
    throws-like { %hash<k> ⚛-= 1 }, X::Multi::NoMatch,
        message => /'infix:<⚛-=>(Int:D, Int:D)'/,
        '⚛-= on a hash element';
    is-deeply @plain, [0], 'a refused update leaves the array alone';
    is-deeply %hash, {k => 1}, '... and the hash';
}

# ---- the existing variable form is unchanged ------------------------------

{
    my atomicint $n = 1;
    $n ⚛+= 4;
    $n ⚛-= 1;
    is $n, 4, 'the scalar-variable form still works';
}

{
    my atomicint @a = 3;
    my $r = do { @a[0] ⚛-= 1 };
    is $r, 2, 'the element form is usable inside a block';
}
