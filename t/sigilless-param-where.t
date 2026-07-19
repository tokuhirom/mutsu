use v6;
use Test;

# A sigilless parameter (`\N`) may carry a `where` constraint, before any
# default value: `\N where * > 0`, `Int \N where BIG`. Regression: mutsu's
# sigilless-param parser handled `is` traits and defaults but not a `where`
# clause, so `sub f(\N where * > 0) {...}` died with "expected ')'".
# (Hit by Prime::Factor's `multi divisors (Int \N where BIG, ...)`.)

plan 7;

# Basic sigilless where constraint.
{
    sub f(\N where * > 0) { N }
    is f(5), 5, 'sigilless param with where * > 0 binds a matching arg';
}

# Typed sigilless where constraint against a constant.
{
    constant BIG = 3 .. *;
    sub g(Int \N where BIG) { N }
    is g(10), 10, 'typed sigilless param with where against a constant';
}

# where + default value together (`\N where C = D`).
{
    sub h(\N where * >= 0 = 42) { N }
    is h(), 42, 'sigilless where + default uses the default';
    is h(7), 7, 'sigilless where + default binds an explicit arg';
}

# The constraint is actually enforced via multi-dispatch.
{
    my @log;
    multi m(\N where * > 0) { @log.push('pos') }
    multi m(\N)            { @log.push('other') }
    m(5);
    m(-1);
    is @log.join(','), 'pos,other', 'where constraint gates multi-dispatch';
}

# A where block ({ ... }) form on a sigilless param.
{
    sub b(\N where { $_ %% 2 }) { N }
    is b(8), 8, 'sigilless param with a where { } block';
}

# Prime::Factor's exact shape: `multi divisors (Int \N where BIG, :s(:$sort))`.
{
    constant BIG = 100 .. *;
    multi divisors(Int \N where BIG, :s(:$sort) = False) { $sort ?? 'sorted' ~ N !! 'big' ~ N }
    is divisors(250), 'big250', 'sigilless where + named alias param parses & runs';
}
