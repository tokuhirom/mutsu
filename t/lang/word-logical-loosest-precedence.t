use Test;

# operators.rakudoc precedence table: the loose word-logicals
# `and`/`or`/`xor`/`andthen`/`orelse`/`notandthen` are the LOOSEST infix
# operators — looser than item assignment (`=`), the comma operator (`,`), and
# the `return`/statement level. So `my $x = 1 and 2` is `(my $x = 1) and 2`,
# and `return True and False` is `(return True) and False` (return fires first).
#
# They still compose INSIDE an expression / parentheses, where there is no
# looser assignment/return to bind first: `say (1 and 2)` is `2`.

plan 18;

# return is tighter than `and`/`or` (return fires, the tail is dead code)
{
    sub f { return True and False }
    is f(), True, 'return True and False -> (return True) and False';
}
{
    sub g { return 1 and die "boom" }
    is g(), 1, 'return 1 and die -> die never runs';
}
{
    sub h { return 5 or die "boom" }
    is h(), 5, 'return 5 or die -> die never runs';
}

# item assignment is tighter than `and`/`or`
{
    my $x = 1 and 2;
    is $x, 1, 'my $x = 1 and 2 -> ($x = 1) and 2';
}
{
    my $y = 5 or die "boom";
    is $y, 5, 'my $y = 5 or die -> ($y = 5) or die';
}
{
    my $z = 0 orelse 9;
    is $z, 0, 'my $z = 0 orelse 9 -> ($z = 0) orelse 9';
}
{
    my $t = 1;
    $t = 2 and 3;
    is $t, 2, '$t = 2 and 3 -> ($t = 2) and 3';
}

# comma is tighter than `and`
{
    my @a = 1, 2 and 3;
    is-deeply @a, [1, 2], 'my @a = 1,2 and 3 -> (my @a = 1,2) and 3';
}

# ...but inside an expression / parens the word-logicals still compose
is (1 and 2), 2, 'and inside parens still composes';
is (1 or 2), 1, 'or inside parens still composes';
is (Nil orelse 7), 7, 'orelse inside parens still composes';

# a bare listop argument is already looser than the word-logicals
{
    my $called = 0;
    sub note-it($v) { $called++; $v }
    my $r = note-it(1) and note-it(2);
    is $called, 2, 'listop-arg word-logical composes at expression level';
}

# compound assignment is also tighter than the word-logicals
{
    my $x = 1;
    $x += 5 and 6;
    is $x, 6, '$x += 5 and 6 -> ($x += 5) and 6';
}
{
    my $x = 1;
    $x += 5 or die "boom";
    is $x, 6, '$x += 5 or die -> die never runs';
}

# a left-associative word-logical chain: only the first leaf is the RHS
{
    my $c = 1 and 2 and 0 or (my $ran = 1);
    is $c, 1, 'my $c = 1 and 2 and 0 or ... -> $c is 1';
    is $ran, 1, 'the trailing `or` branch ran ($c chain was falsy)';
}

# `return` value stops before the word-logical (mixed and/or tail is all dead)
{
    sub k { return 3 or die "boom" and die "boom2" }
    is k(), 3, 'return 3 or die and die -> return 3, whole tail dead';
}

# array declaration: word-logical wraps the whole (comma) list assignment
{
    my @a = 1, 2, 3 and 4;
    is-deeply @a, [1, 2, 3], 'my @a = 1,2,3 and 4 -> (my @a = 1,2,3) and 4';
}
