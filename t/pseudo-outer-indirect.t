use v6;
use Test;

plan 12;

# `$::($name)::x` is the same lookup as `$OUTER::x`, only spelled with a package
# name that does not exist until run time. The invariant this file pins is that
# the two spellings never drift: the indirect form must be answered against the
# LEXICAL scope chain at the deref site, exactly as the compiler answers the
# literal one. Assertions are written as "indirect eqv direct" wherever the
# value itself is not the interesting part.

my $outer  = 'OUTER';
my $outers = 'OUTERS';

my $x = 102;
my $y = 103;
{
    my $x = 104;
    is $::($outer)::x, 102, '::("OUTER") reads the next outer scope';
    is $::($outer)::x, $OUTER::x, '::("OUTER") agrees with the literal spelling';

    {
        my $x = 105;
        my $y = 106;
        is $::($outers)::y, 103, '::("OUTERS") keeps going until a match';
        is $::($outers)::y, $OUTERS::y, '::("OUTERS") agrees with the literal';
        is $::($outer)::($outer)::x, 102, '::("OUTER")::("OUTER") walks two scopes';
        is $::($outer)::($outer)::x, $OUTER::OUTER::x,
            '::("OUTER")::("OUTER") agrees with the literal';
    }
}

# OUTER names exactly ONE scope, so a name only an outer-outer scope declares is
# not found -- the indirect form must not soften that into an OUTERS cascade.
{
    my $only-here = 'deep';
    {
        {
            ok !defined($::($outer)::only-here), '::("OUTER") does not cascade';
            is $::($outers)::only-here, 'deep', '::("OUTERS") does cascade';
        }
    }
}

# A block is itself a scope, so a closure's OUTER:: is the block that encloses
# it lexically -- never its caller's scope, even though the caller's blocks are
# what the runtime scope stack holds by the time the closure runs.
sub call-it($f) { my $c = 'callee-scope'; $f() }
{
    my $c = 'outer-scope';
    {
        my $c = 'enclosing-scope';
        is call-it({ $::($outer)::c }), 'enclosing-scope',
            '::("OUTER") in a closure is lexical, not the caller';
        is call-it({ $::($outer)::c }), call-it({ $OUTER::c }),
            '... and agrees with the literal spelling';
    }
}

# Parameters are declarations of their routine's scope.
sub with-param($p) { { $::($outer)::p } }
is with-param(42), 42, '::("OUTER") sees a parameter of the enclosing routine';

# The pseudo-package name is a plain expression, not a literal.
{
    my $z = 7;
    {
        my $z = 8;
        is $::('OUT' ~ 'ER')::z, 7, 'the package name can be computed';
    }
}

# vim: expandtab shiftwidth=4
