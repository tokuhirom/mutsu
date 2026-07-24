use v6;
use Test;

# The method form `$format.sprintf(*@args)` has a slurpy signature, so a single
# positional container argument spreads across the directives exactly like the
# sub form `sprintf($format, @args)`. Previously mutsu's native 1-arg `.sprintf`
# fast path passed the whole array as ONE value, so `"%d".sprintf([42])` gave 0
# and `"%s-%s".sprintf([1,2])` gave "1 2-".

plan 14;

is "%d".sprintf([42]),        '42',      '"%d".sprintf([42]) spreads the array';
is "%s-%s".sprintf([1, 2]),   '1-2',     '"%s-%s".sprintf([1,2]) spreads to two args';
is "%d-%d-%d".sprintf([1,2,3]), '1-2-3', 'three-element array spreads to three args';
is "%s and %s".sprintf(["a", "b"]), 'a and b', 'string elements spread';

# A list literal spreads the same way.
is "%s-%s".sprintf((1, 2)),   '1-2',     'a List argument spreads';

# The method form agrees with the sub form.
is "%s-%s".sprintf([1, 2]), sprintf("%s-%s", [1, 2]),
    'method form matches the sub form';

# A single non-container argument still works (unchanged fast path).
is "%d".sprintf(42),      '42',   'single Int arg';
is "%s".sprintf("hi"),    'hi',   'single Str arg';
is "%05.2f".sprintf(3.14159), '03.14', 'formatting flags on a single arg';

# An array bound to a variable spreads too.
{
    my @a = 7, 8;
    is "%d/%d".sprintf(@a), '7/8', 'an @-var argument spreads';
}

# Argument-count mismatch is still detected after spreading.
{
    dies-ok { "%d".sprintf([1, 2, 3]) },
        'too many spread args dies (directive/arg count mismatch)';
    dies-ok { "no directives".sprintf([1, 2]) },
        'extra args with no directives dies';
}

# A bare type object element still gets the string-context warning (from #5336).
{
    my $r;
    { $r = quietly "%s".sprintf(Int); }
    is $r, '', 'a bare type object stringifies to "" (with a warning)';
}

# A single %s consuming a multi-element array is an arg-count mismatch (each
# element is a separate arg after spreading), matching the sub form.
dies-ok { "%s".sprintf([1, 2, 3]) }, '"%s".sprintf([1,2,3]) is an arg-count mismatch';
