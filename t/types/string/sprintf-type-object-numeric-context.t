use v6;
use Test;

# A bare type object rendered by a numeric directive (%d %i %u %b %o %x %X %c
# %e %f %g) coerces to 0 with rakudo's "Use of uninitialized value of type X in
# numeric context" warning (matching `+Int`), rather than the mutsu `(Int)`
# gist. The `%s` string-context warning is covered by
# t/sprintf-type-object-str-context.t.

plan 10;

# Value coercion (warnings quieted; the warning itself is checked below).
is quietly(sprintf("%d", Int)),      "0",        '%d on Int -> 0';
is quietly(sprintf("%f", Int)),      "0.000000", '%f on Int -> 0.000000';
is quietly(sprintf("%x", Str)),      "0",        '%x on Str -> 0';
is quietly(sprintf("%e", Int)),      "0.000000e+00", '%e on Int -> exponent form';
is quietly(sprintf("%d|%d", Int, 5)),"0|5",      'mixed type-object / value';

# Concrete values are unaffected (and do NOT warn).
is sprintf("%d", 42),   "42", 'concrete Int still renders';

# A user `.Int` dispatches even for the type object (no warning).
class C { method Int { 7 } }
is sprintf("%d", C),  "7", 'user .Int on a type object dispatches (no warning)';

# The numeric-context warning is actually emitted, with the right wording
# (no "Methods .^name..." suffix — that is string-context only).
{
    my @warnings;
    my $r;
    {
        CONTROL { when CX::Warn { @warnings.push(.message); .resume } }
        $r = sprintf("%d", Int);
    }
    is $r, "0", 'resumes with 0';
    ok @warnings.grep(*.contains("numeric context")), 'numeric-context warning emitted';
    nok @warnings.grep(*.contains("Methods .^name")), 'no string-context Methods suffix';
}
