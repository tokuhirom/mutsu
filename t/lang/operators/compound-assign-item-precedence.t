use Test;

plan 14;

# A compound assignment `op=` whose base operator is tighter than the comma
# has item-assignment precedence, whatever the lvalue's sigil: the comma
# after its right operand ends the assignment (#9566). Only `,=` itself takes
# the comma list as its operand.

{
    my $f = 0;
    my @r = ($f += 5, 9);
    is $f, 5, 'parenthesized `$f += 5, 9` assigns only 5';
    is @r.elems, 2, '... and the list keeps both items';
}

{
    my @a = 1;
    my @r = (@a += 5, 9);
    is @a, [6], '`@a += 5, 9` adds only 5 (numeric @a is 1)';
    is @r[1], 9, '... and 9 is the second list item';
}

{
    my $x = 0;
    sub g(*@a) { @a.elems }
    is g($x += 5, 9), 2, '`$x += 5, 9` is two call arguments';
    is $x, 5, '... and $x got 5';
}

{
    my $x = 2;
    my @r = ($x **= 2 + 1, 0);
    is $x, 8, 'a tighter infix stays in the right operand';
}

{
    my @a = 1;
    my @r = (@a ,= 5, 9);
    is @a.elems, 2, '`,=` still takes the whole comma list';
}

# The statement forms: the trailing items are sunk.
{
    my $x;
    $x //= 1, 2;
    is $x, 1, '`$x //= 1, 2` as a statement assigns 1';
}

{
    my $s = 'a';
    $s ~= 'b', 'c';
    is $s, 'ab', '`$s ~= "b", "c"` appends only "b"';
}

{
    my %h;
    %h<a> += 1, 2;
    is %h<a>, 1, '`%h<a> += 1, 2` adds only 1';
}

{
    my @a = 1, 2;
    @a[0] += 1, 2;
    is @a, [2, 2], '`@a[0] += 1, 2` adds only 1';
}

{
    my $x = 1;
    $x += 2 for 1..3;
    is $x, 7, 'a statement modifier still follows the right operand';
}

{
    my $x = 10;
    my $y = ($x div= 3, 1);
    is $x, 3, 'a word operator (`div=`) is item assignment too';
}
