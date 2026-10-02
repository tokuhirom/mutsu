use Test;

# A sigil-less `constant` holding an Array/Hash is a container whose
# elements are writable: element assignment, `++`/`--`, compound assignment
# and `:delete` all write through the constant's term binding (#10468: the
# element-lvalue root of a sigil-less term now resolves to its term key, not
# to its spelling).

plan 14;

{
    constant c = [1, 2, 3];
    c[0] = 7;
    is c, [7, 2, 3], 'element assignment';
    c[1]++;
    is c, [7, 3, 3], 'postfix increment';
    ++c[2];
    is c, [7, 3, 4], 'prefix increment';
    c[2]--;
    --c[1];
    is c, [7, 2, 3], 'postfix and prefix decrement';
    c[0] += 5;
    is c, [12, 2, 3], 'compound assignment';
    c[0, 1] = 8, 9;
    is c, [8, 9, 3], 'slice assignment';
    c[5] //= 6;
    is c[5], 6, 'defined-or assignment autovivifies';
}

{
    constant h = { a => 1 };
    h<b> = 2;
    h<a>++;
    is h<a b>, (2, 2), 'hash element assignment and increment';
    h<a>:delete;
    is h.keys.sort, ('b',), 'hash element :delete';
}

{
    constant n = [[1, 2], [3]];
    n[0][1] = 9;
    n[1][0]++;
    is n, [[1, 9], [4]], 'nested element assignment and increment';
}

{
    sub f { constant k = [1, 2]; k[0] = 5; k }
    is f(), [5, 2], 'a constant inside a routine';
}

{
    my \x = [1, 2, 3];
    x[0] = 7;
    x[1]++;
    is x, [7, 3, 3], 'a sigil-less my binding still writes through';
}

{
    my $s = "abc";
    ($s).substr-rw(0, 1) = "Q";
    is $s, "Qbc", 'parenthesized method-lvalue statement writes back';
    my \y = $s;
    (y).substr-rw(1, 1) = "Z";
    is $s, "QZc", 'parenthesized sigil-less method-lvalue writes back';
}
