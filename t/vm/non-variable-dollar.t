use Test;

plan 9;

# A bare `$` in an interpolating string must be backslashed.
throws-like '"$"',       X::Backslash::NonVariableDollar;
throws-like '"a$"',      X::Backslash::NonVariableDollar;
throws-like '"$ x"',     X::Backslash::NonVariableDollar;
throws-like '"price $"', X::Backslash::NonVariableDollar;

# A backslashed dollar is a literal `$`.
is "money \$5", 'money $5', 'backslashed dollar is literal';

# Normal variable interpolation is unaffected.
my $x = 42;
is "v=$x", 'v=42', 'scalar interpolation works';

# A `$` before another sigil is a contextualizer prefix, not a bare dollar:
# the following sigil-variable interpolates (here undeclared -> X::Undeclared).
throws-like '"$@whoopsies[]"', X::Undeclared, symbol => '@whoopsies';
throws-like '"$%whoopsies{}"', X::Undeclared, symbol => '%whoopsies';

# A `$` before a sigil must not be rejected as a bare dollar (full deref
# interpolation of `$@arr` is a separate, pre-existing limitation).
lives-ok { my @arr = 1, 2, 3; EVAL '"$@arr"' },
    'sigil-prefixed dollar is not rejected as a non-variable dollar';
