use Test;

plan 5;

# CALLER::<$x> stash-subscript form resolves a caller-frame lexical, exactly
# like the $CALLER::x symbolic form. Accessing a non-dynamic variable through
# CALLER throws X::Caller::NotDynamic.

# Non-dynamic variable: both forms throw X::Caller::NotDynamic.
throws-like 'sub f() { CALLER::<$x> }; my $x; f',
    X::Caller::NotDynamic, symbol => '$x',
    'CALLER::<$x> on a non-dynamic var throws X::Caller::NotDynamic';

throws-like 'sub f() { $CALLER::x }; my $x; f',
    X::Caller::NotDynamic, symbol => '$x',
    '$CALLER::x on a non-dynamic var throws X::Caller::NotDynamic';

# Dynamic variable: the subscript form resolves the caller's value.
sub get-bar() { CALLER::<$*bar> }
{
    my $*bar = 93;
    is get-bar(), 93, 'CALLER::<$*bar> resolves a dynamic caller var';
}

# The symbol field reflects the accessed variable name.
throws-like 'sub g() { CALLER::<$abc> }; my $abc; g',
    X::Caller::NotDynamic, symbol => '$abc',
    'symbol field reflects the accessed variable name';

# Nested CALLER::CALLER::<$*v> walks two frames for a dynamic var.
sub inner() { CALLER::CALLER::<$*v> }
sub outer() { inner() }
{
    my $*v = 'deep';
    is outer(), 'deep', 'CALLER::CALLER::<$*v> walks two caller frames';
}
