use Test;

# A closure built on the right-hand side of `my (...) = RHS` captures the
# variable the declaration declares, so it sees the value the declaration
# assigns (#18414).

plan 9;

my &get;
{
    my ($a, $c) = 5, (&get = { $a });
    is get(), 5, 'a closure in the RHS sees the assigned scalar';
    $a = 6;
    is get(), 6, '... and later writes to it';
}

my ($b, $d) = (&get = { $b }), 3;
isa-ok get(), Block, 'a closure assigned to its own target sees itself';

{
    my ($x, @rest) = 1, (&get = { @rest.elems }), 3;
    is get(), 2, 'an array target the closure reads is the assigned one';
}

{
    my ($p, %h) = (&get = { %h<k> }), 'k', 'v';
    is get(), 'v', 'a hash target the closure reads is the assigned one';
}

my $outer = 5;
{
    my ($outer, $y) = $outer, 2;
    is $outer.raku, 'Any', 'the RHS still reads the new (default) variable';
}

my ($m, $n) is default(7) = 1;
is $n, 7, 'a group default still applies to a target the RHS did not reach';

# The RHS may bind a same-named code variable; the scalar target stays
# assignable.
{
    my ($e, $go) = do { my &go := { 1 }; (2, &go) };
    is $e, 2, 'a target next to a same-named `&go` bound in the RHS';
}
{
    my $go;
    my &go := { 1 };
    $go = 3;
    is $go, 3, 'binding `&go` leaves a same-named `$go` writable';
}
