use Test;

# A bare `pop @a;` / `shift @a;` statement (the listop/sub form) on an empty
# array sinks its return value in sink context. Sinking the resulting Failure
# throws X::Cannot::Empty — matching the method form `@a.pop;` / `@a.shift;`.
# Regression: the sub form compiled to a plain `Pop` (silent discard) instead
# of `SinkPop`, so the Failure vanished. (Trailing `True` keeps the mutating
# call in non-tail / sink position.)

plan 9;

# --- sub form throws in sink context ---
throws-like { my @a; pop @a; True },   X::Cannot::Empty, 'pop @a; on empty throws in sink context';
throws-like { my @a; shift @a; True }, X::Cannot::Empty, 'shift @a; on empty throws in sink context';

# --- method form still throws (unchanged) ---
throws-like { my @a; @a.pop; True },   X::Cannot::Empty, '@a.pop; on empty throws (method form, unchanged)';

# --- assigning the Failure does not throw; it is a Failure value ---
{
    my @a;
    my $x = pop @a;
    is $x.WHAT.^name, 'Failure', 'assigned pop-of-empty is a Failure (not thrown when captured)';
    nok $x.defined, 'the captured Failure is undefined';
}

# --- non-empty sub-form pop/shift still work and mutate ---
{
    my @a = 1, 2, 3;
    pop @a;
    is @a, [1, 2], 'pop @a; on non-empty mutates in place';
    shift @a;
    is @a, [2], 'shift @a; on non-empty mutates in place';
}

# --- push/unshift sub form still work (never Failure) ---
{
    my @a;
    push @a, 10, 20;
    unshift @a, 5;
    is @a, [5, 10, 20], 'push/unshift @a; still work as statements';
}

# --- a non-tail non-empty pop does not throw (Failure only on empty) ---
lives-ok { my @a = 1, 2; pop @a; True }, 'non-empty pop in sink position does not throw';
