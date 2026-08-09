use v6;
use Test;

plan 14;

# Out-of-range .UInt coercion returns a soft X::OutOfRange Failure (rakudo
# behavior), not a thrown exception.
{
    my $x = "-1".UInt;
    isa-ok $x, Failure, '"-1".UInt returns a Failure';
    is $x.exception.^name, 'X::OutOfRange', '... carrying X::OutOfRange';
    is $x.exception.message, 'Coercion to UInt out of range. Is: -1, should be in 0..^Inf',
        '... with the rakudo message';
    is $x.exception.what, 'Coercion to UInt', '... .what';
    is $x.exception.got, -1, '... .got';
    is $x.exception.range, '0..^Inf', '... .range';
}

{
    my $y = (-5).UInt;
    isa-ok $y, Failure, '(-5).UInt returns a Failure too';
}

# Using the Failure throws.
throws-like { "-1".UInt + 1 }, X::OutOfRange, 'using the Failure throws';

# Storing the Failure via an element assignment statement does not throw...
{
    my %u;
    my @a;
    lives-ok { %u{'p'} = "-1".UInt; 1 }, 'hash element assignment stays soft';
    lives-ok { @a[0] = "-1".UInt; 1 }, 'array element assignment stays soft';
    lives-ok {
        my %v;
        %v{'p'} = "-1".UInt if True;
        1;
    }, 'if-modifier assignment stays soft';
}

# ... but a topicalizing `with` modifier sinks the value and throws (rakudo).
throws-like {
    my %u;
    %u{'p'} = .UInt with "-1";
}, X::OutOfRange, 'with-modifier assignment throws';

# Inside a regex { ... } code block, statements are wanted, not sunk: the
# stored Failure stays soft (Cro's generated route matcher relies on this).
{
    my %u;
    lives-ok { "x" ~~ /x { %u{'p'} = .UInt with "-1"; } / },
        'regex code-block assignment stays soft even with a with-modifier';
    isa-ok %u{'p'}, Failure, '... and the Failure landed in the hash';
}
