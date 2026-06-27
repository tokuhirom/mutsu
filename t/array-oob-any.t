use Test;

plan 24;

# An out-of-range read on a real Array yields the `Any` type object (not Nil),
# while a List yields Nil.
{
    my @a = 1, 2, 3;
    is @a[11].^name, 'Any', 'untyped Array OOB is Any';
    is [1, 2, 3][11].^name, 'Any', 'Array literal OOB is Any';
    is (1, 2, 3)[11].raku, 'Nil', 'List OOB is Nil';
    is ().[0].raku, 'Nil', 'empty List OOB is Nil';

    my Str @s = 'a', 'b';
    is @s[11].^name, 'Str', 'typed Array OOB is the element type';
}

# A type object coerces to "" (Str context) and 0 (numeric context).
{
    ok Any eq '', 'Any eq ""';
    ok Str eq '', 'Str eq ""';
    ok Int == 0, 'Int == 0';
    ok Any == 0, 'Any == 0';
    my @foo;
    ok @foo[0] eq '', 'Array OOB eq ""';
    ok @foo[0] == 0, 'Array OOB == 0';
    ok @foo[0] ne 'f', 'Array OOB ne "f"';
}

# An array-valued subscript is always a slice (a 1-element list, not a scalar).
{
    my @a = 10, 20, 30;
    my @b = 1;
    is-deeply @a[@b], (20,), '@a[@b] single-elem slice is a list';
    is-deeply @a[(1,)], (20,), '@a[(1,)] is a list';
    is-deeply @a[1,], (20,), '@a[1,] is a list';
    is @a[1], 20, '@a[1] scalar subscript stays scalar';
    is-deeply @a[@b]:!v, (20,), '@a[@b]:!v is a list';

    my @B = 11;
    is-deeply @a[@B], (Any,), 'missing array slice yields (Any,)';
    is-deeply @a[@B]:!v, (Any,), 'missing array :!v yields (Any,)';
}

# List-destructuring assignment fills missing typed slots with the type default.
{
    my Str ($a) = ();
    is $a.^name, 'Str', 'my Str ($a) = () gives the Str default';
    my ($b) = ();
    is $b.^name, 'Any', 'untyped destructure missing is Any';
    my Int ($c) = ();
    is $c.^name, 'Int', 'my Int ($c) = () gives the Int default';
    my ($d, $e) = 5;
    is $e.^name, 'Any', 'untyped short destructure missing is Any';
    my Str ($f) = 'x';
    is $f, 'x', 'present value passes through';
}
