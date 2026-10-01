use Test;

plan 5;

# A named routine's call `g()` to a free `&g` uses the `&g` it closes over,
# not a same-named `my &g` in its caller (#10483).
my &g = { "outer" };
sub helper { g() }
{
    my &g = { "inner" };
    is helper(), "outer", "mainline sub keeps its outer &g";
}

sub n {
    my &h = { "outer" };
    sub helper2 { h() }
    { my &h = { "inner" }; helper2() }
}
is n(), "outer", "routine-nested sub keeps its outer &h";

sub m {
    my &h = { "outer" };
    my sub helper3 { h() }
    { my &h = { "inner" }; helper3() }
}
is m(), "outer", "my sub keeps its outer &h";

my $v = "outer";
sub scalar-helper { $v }
{
    my $v = "inner";
    is scalar-helper(), "outer", "scalar control";
}

sub p {
    my &h = { "outer" };
    sub helper4 { h() }
    helper4()
}
is p(), "outer", "plain call from the declaring routine";
