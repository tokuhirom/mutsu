use v6;
use Test;

# Regression: a `Bool` default value conforms to an `Int`/`Numeric`/`Real`
# parameter constraint, because `Bool` enumerates to `Int` (`enum Bool does Int`)
# in raku. mutsu's compile-time default-type check wrongly rejected
# `Int :$wrap = False` with X::Parameter::Default::TypeCheck.
# Surfaced loading Zef::CLI:  multi sub MAIN('search', Int :$wrap = False, ...)

plan 8;

# The exact zef shape: Int-typed named param with a Bool default.
{
    sub f(Int :$wrap = False) { $wrap }
    is f().^name, 'Bool', 'Int :$wrap = False defaults to the Bool value';
    is f(wrap => 5), 5, 'passing an Int still binds as Int';
}

# Bool default also conforms to Numeric / Real.
{
    sub g(Numeric :$x = False) { $x }
    is g().^name, 'Bool', 'Numeric :$x = False accepted';
    sub h(Real :$x = False) { $x }
    is h().^name, 'Bool', 'Real :$x = False accepted';
}

# True works the same way.
{
    sub t(Int :$on = True) { $on }
    is t(), True, 'Int :$on = True accepted';
}

# Genuinely incompatible defaults are still rejected at compile time.
{
    dies-ok { EVAL 'sub f(Int :$x = "str") { }' },
        'Str default for Int param still rejected';
    dies-ok { EVAL 'sub f(Str :$x = 5) { }' },
        'Int default for Str param still rejected';
    dies-ok { EVAL 'sub f(Num :$x = False) { }' },
        'Bool default for Num param rejected (Bool is not a Num)';
}
