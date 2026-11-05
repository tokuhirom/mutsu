use Test;
plan 5;

# A too-wide Int unboxed into a native integer carries rakudo's full message,
# whatever site unboxes it (declaration, parameter, uint parameter).
my $hint = 'Did you mix int and Int or literals?';

{
    my $msg;
    try { my int $y = 2**100; CATCH { default { $msg = .message } } }
    is $msg, "Cannot unbox 101 bit wide bigint into native integer. $hint",
        'my int $y = 2**100';
}
{
    my $msg;
    sub f(int $x) { $x }
    try { f 2**64; CATCH { default { $msg = .message } } }
    is $msg, "Cannot unbox 65 bit wide bigint into native integer. $hint",
        'an int parameter given 2**64';
}
{
    my $msg;
    sub g(uint $x) { $x }
    try { g 2**64; CATCH { default { $msg = .message } } }
    is $msg, "Cannot unbox 65 bit wide bigint into native integer. $hint",
        'a uint parameter given 2**64';
}
{
    my $msg;
    my int $z;
    try { $z = 2**70; CATCH { default { $msg = .message } } }
    is $msg, "Cannot unbox 71 bit wide bigint into native integer. $hint",
        'assignment to a declared int';
}
{
    my int $ok = 2**62;
    is $ok, 4611686018427387904, 'a value that fits is still accepted';
}
