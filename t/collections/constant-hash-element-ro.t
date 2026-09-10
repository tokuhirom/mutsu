use v6;
use Test;

plan 14;

# A `constant %M` is an immutable Map: element assignment must die with
# X::Assignment::RO, mirroring raku. Previously mutsu treated every readonly
# `%`-var as a `:=`-bound (writable) hash and silently mutated the constant.
# The `:=` desugar records a dedicated `__mutsu_bound::%name` marker (#3255),
# which now distinguishes a writable bound hash from an immutable constant.

# --- constant hash element assignment dies ---
{
    my constant %M = (a => 1, b => 2);
    dies-ok { %M<a> = 9 }, 'constant hash element assign dies';
    is %M<a>, 1, 'constant hash value unchanged after failed assign';
}

# --- error type and message ---
{
    try {
        my constant %M = (a => 1);
        %M<a> = 9;
    }
    my $err = $!;
    isa-ok $err, X::Assignment::RO, 'throws X::Assignment::RO';
    is $err.message, 'Cannot modify an immutable Int (1)',
        'message reports the immutable element value';
}

# --- absent key also dies ---
{
    my constant %M = (a => 1);
    dies-ok { %M<zzz> = 9 }, 'constant hash assign to absent key dies';
}

# --- := element bind into a constant dies ---
{
    my constant %M = (a => 1);
    my $x = 5;
    dies-ok { %M<a> := $x }, 'binding an element of a constant hash dies';
}

# --- a `:=`-bound hash stays writable (regression guard for #3255) ---
{
    my %a = (x => 1);
    my %b := %a;
    lives-ok { %b<y> = 2 }, 'bound hash element assign lives';
    is %a<y>, 2, 'bound hash element write propagates to source';
    lives-ok { %b = (z => 9) }, 'bound hash whole reassign lives (#3255)';
    is %a<z>, 9, 'bound hash whole reassign propagates';
}

# --- plain hash and hash params remain writable ---
{
    my %h = (a => 1);
    lives-ok { %h<b> = 2 }, 'plain hash element assign lives';
    is %h<b>, 2, 'plain hash element write applied';
}

{
    sub f(%h) { %h<x> = 1 }
    my %g = (a => 1);
    lives-ok { f(%g) }, 'hash param element assign lives';
    is %g<x>, 1, 'hash param element write propagates to caller';
}

done-testing;
