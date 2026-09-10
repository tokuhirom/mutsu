use Test;

# A multi *method* whose parameter is a subset type must be preferred over a
# candidate typed with the subset's base type (a subset is narrower than its
# base). Previously the method-multi ranker gave a subset the "unknown type"
# distance, so it wrongly lost to a bare `Any`. Also `$x ~~ Subset:D` (a subset
# with a definiteness smiley used as a term) must type-check, not string-compare.
# S12-subset/subtypes.t test 83.

plan 6;

# Method multi: subset beats its base.
{
    subset T of List where *[0] eqv 1;
    class R {
        multi method f(T $xs)   { 'T' }
        multi method f(Any $xs) { 'Any' }
    }
    is R.f([1, 2]), 'T',   'method multi picks the subset candidate';
    is R.f([2, 2]), 'Any', 'method multi falls back to base when subset fails';
}

# Method multi with :D subset smiley + recursion (the roast form).
{
    my @results;
    subset T2 of List where *[0] eqv 1;
    class R2 {
        multi method f(T2:D $xs)  { @results.push('T:D'); self.f(42) }
        multi method f(Any:D $xs) { @results.push($xs) }
    }
    R2.f([1, 2]);
    R2.f([2, 2]);
    is @results.raku, '["T:D", 42, [2, 2]]', 'multi with :D subset dispatches in order';
}

# A subset with :D smiley as a smartmatch term.
{
    subset Ev of Int where * %% 2;
    ok  (4 ~~ Ev:D),  'defined value matches Subset:D';
    nok (3 ~~ Ev:D),  'non-matching value fails Subset:D';
    nok (Int ~~ Ev:D), 'type object fails Subset:D (needs definite)';
}
