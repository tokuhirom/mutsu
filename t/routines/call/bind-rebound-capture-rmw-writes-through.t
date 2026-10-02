use Test;

# A lexical a closure captured and the frame later rebinds with `:=` is boxed
# in a binding cell whose content is the variable's current container (#9237).
# A read-modify-write in the closure (`$x++`, `$x += 1`, `$x ~= ...`) must step
# that container. It used to overwrite the binding cell with the bare new
# value, which cut `$x` loose from the container it is bound to (#10826).

plan 10;

{
    my $x = 1;
    my $s = sub { $x++ };
    my $y = 7;
    $x := $y;
    $s();
    is $x, 8, 'postfix ++ in a closure steps the rebound variable';
    is $y, 8, '... and writes through to the bound container';
    $y = 20;
    is $x, 20, '... which stays bound afterwards';
}

{
    my $x = 1;
    my $s = sub { ++$x; $x--; --$x };
    my $y = 7;
    $x := $y;
    $s();
    is $y, 6, 'prefix ++, postfix -- and prefix -- write through';
}

{
    my $x = 1;
    my $s = sub { $x *= 3 };
    my $y = 7;
    $x := $y;
    $s();
    is $y, 21, 'a compound assignment writes through';
}

{
    my $x = 'a';
    my $s = sub { $x ~= 'b' };
    my $y = 'y';
    $x := $y;
    $s();
    is $y, 'yb', 'an in-place concatenation writes through';
}

{
    my $x = 1;
    my $y;
    $x := $y;
    $x = 1;
    my $s = sub { $x-- };
    $s();
    is $x, 0, 'a rebind before the capture: -- steps the variable';
    is $y, 0, '... and its bound container';
}

# The issue's shape: the rebind is of a different `$x` in a sibling block that
# shares the frame slot.
{
    { my $x = 1; my $y; $x := $y; }
    {
        my $x = 1;
        sub s6 { $x++; $x }
        is s6(), 2, 'a sibling block rebind does not break ++ in a named sub';
        is $x, 2, '... and the increment is visible in the block';
    }
}
