use Test;

plan 7;

# A runtime mixin (`$x but role { method Bool {...} }`) must boolify via the
# mixed-in `Bool` method in boolean context (`?`, `so`, `if`, `unless`), exactly
# like a class with a user-defined `Bool` — not via the base value's truthiness.

my $falsey = 42 but role { method Bool { False } };
is        $falsey.Bool, False, '.Bool method returns the mixed-in value';
nok       ?$falsey,             'prefix ? uses the mixed-in Bool';
nok       so $falsey,          'so uses the mixed-in Bool';
is (?$falsey ?? 'y' !! 'n'), 'n', 'ternary boolean context uses mixed-in Bool';
{
    my $hit = 'else';
    if $falsey { $hit = 'then' }
    is $hit, 'else', 'if uses the mixed-in Bool';
}

# A mixin with a truthy Bool stays true; the base value is unaffected.
my $truthy = 0 but role { method Bool { True } };
ok ?$truthy, 'prefix ? uses a True-returning mixed-in Bool over a falsey base';

# A plain value with no mixed-in Bool keeps its ordinary truthiness.
ok ?(42 but role { method Str { "x" } }), 'mixin without Bool keeps base truthiness';
