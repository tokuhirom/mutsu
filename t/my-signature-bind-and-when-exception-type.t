use Test;

plan 9;

# `my :($a, $b) := expr` — signature-literal destructuring bind. Binds the
# positional targets exactly like `my ($a, $b) := expr`.
{
    my :($owner, $key) := (10, 20);
    is $owner, 10, 'my :(...) binds first positional';
    is $key, 20, 'my :(...) binds second positional';
}

{
    sub pair-parts { (3, 7) }
    my :($a, $b) := pair-parts();
    is $a + $b, 10, 'my :(...) binds from a sub call';
}

{
    my :($x) := (42,);
    is $x, 42, 'my :(...) single positional';
}

# `when <X:: builtin exception type> { ... }` must resolve the type rather than
# treating the qualified name as an undeclared block-gobbling function call.
{
    my $r = do given X::IO {
        when X::IO { 'io' }
        default { 'other' }
    };
    is $r, 'io', 'when X::IO resolves as a known exception type';
}

{
    my $r = do given X::CompUnit::UnsatisfiedDependency {
        when X::CompUnit::UnsatisfiedDependency { 'yes' }
        default { 'no' }
    };
    is $r, 'yes', 'when X::CompUnit::UnsatisfiedDependency resolves';
}

# In a CATCH block, a `when` on a real deep-namespaced exception type parses.
{
    my $caught = '';
    sub boom {
        CATCH {
            when X::AdHoc { $caught = 'adhoc'; return }
        }
        die 'kaboom';
    }
    boom();
    is $caught, 'adhoc', 'when inside CATCH matches a real exception type';
}

# A genuinely-undeclared X:: name still raises the block-gobbling error at parse
# time, matching Rakudo.
{
    my $err;
    try {
        EVAL 'when X::TotallyUndeclared::Nope { 1 }';
        CATCH { default { $err = .^name } }
    }
    ok $err.defined, 'undeclared X:: name in when still raises at parse time';
}

# The scalar `$!attr` twigil path was never affected; sanity-check a plain
# parenthesized bind still works alongside the signature form.
{
    my ($p, $q) := (1, 2);
    is "$p $q", '1 2', 'plain my (...) bind still works';
}
