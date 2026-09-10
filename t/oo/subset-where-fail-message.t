use Test;

# A subset whose `where` predicate fails via `fail "msg"` surfaces that custom
# message as the type-check exception on assignment, instead of the generic
# "Type check failed ...; expected X, got Y" (S12-subset/subtypes.t test 25).

plan 4;

# Placeholder-block predicate (the roast form).
{
    subset Even of Int where { $^num %% 2 or fail "$num is not even" };
    throws-like { my Even $e = 1 }, Exception, :message("1 is not even"),
        'fail() message from a $^-placeholder where surfaces on assignment';
}

# Topic (`$_`) predicate.
{
    subset Big of Int where { $_ > 100 or fail "$_ too small" };
    throws-like { my Big $b = 3 }, Exception, :message("3 too small"),
        'fail() message from a $_ where surfaces on assignment';
}

# A `where` that fails *without* fail() still gives the generic type-check error.
{
    subset Odd of Int where { $_ % 2 == 1 };
    throws-like { my Odd $o = 2 }, X::TypeCheck,
        'a plain failing where still throws X::TypeCheck (no custom message)';
}

# Smartmatch against such a subset is just False (the fail is caught), not a throw.
{
    subset Ev of Int where { $_ %% 2 or fail "nope" };
    nok (3 ~~ Ev), 'smartmatch against a fail()-where subset is False, not a throw';
}
