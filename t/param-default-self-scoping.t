use Test;

# A parameter is in scope (as its undefined type object) within its own
# default expression, shadowing any outer symbol of the same name. See
# roast S06-signature/code.t test 8 (RT #67932) and the general Raku rule.

plan 7;

# Self-reference: the inner `&foo` default refers to the (undefined)
# parameter, not the outer routine `foo`.
{
    my $tracker;
    sub foo(&foo = &foo) { $tracker = &foo }
    foo;
    nok $tracker.defined, 'inner &foo default is the undefined param, not outer sub';
}

# Scalar self-reference is undefined.
{
    my $x = 10;
    sub f($x = $x) { $x }
    nok f().defined, 'scalar $x = $x default is undefined (self-shadow)';
}

# Earlier parameters remain visible in later defaults.
{
    sub g($a = 5, $b = $a) { "$a $b" }
    is g(), '5 5', 'earlier param visible in later default';
    is g(2), '2 2', 'earlier param value flows to later default';
}

# An outer symbol of a *different* name is visible in a default.
{
    my $y = 99;
    sub h($z = $y) { $z }
    is h(), 99, 'outer symbol of different name visible in default';
}

# Named parameter self-reference also shadows.
{
    my $w = 7;
    sub k(:$w = $w) { $w }
    nok k().defined, 'named param :$w = $w default is undefined (self-shadow)';
}

# Passing an argument still binds normally (default not used).
{
    my $x = 10;
    sub m($x = $x) { $x }
    is m(42), 42, 'explicit argument binds, default skipped';
}
