use Test;

# A subset declaration may place the `is export` (and other `is`) traits AFTER
# the `of BaseType`, not only before it:  `subset S of T is export where …`.
# mutsu only parsed the `is` traits before `of`, so a trait after `of` was left
# unparsed and the following `where …` was stranded as a bare statement (which
# then failed to parse). Found via the real-distribution compat sweep
# (Business::CreditCard, docs/dist-compat-sweep.md): it declares
# `my subset CreditCard of Str() is export where !*.contains(/…/)`.

plan 8;

# `of` then `is export` then `where` — the Business::CreditCard order
{
    my subset Pos of Int is export where * > 0;
    ok 5 ~~ Pos, 'subset (of … is export where) accepts a matching value';
    nok -5 ~~ Pos, 'subset (of … is export where) rejects a non-matching value';
}

# `is export` before `of` still works (unchanged path)
{
    my subset Neg is export of Int where * < 0;
    ok -3 ~~ Neg, 'subset (is export … of … where) accepts';
    nok 3 ~~ Neg, 'subset (is export … of … where) rejects';
}

# `of … is export` with no where clause
{
    my subset AStr of Str is export;
    ok 'hi' ~~ AStr, 'subset (of … is export) with no where accepts a Str';
    nok 5 ~~ AStr, 'subset (of … is export) rejects a non-Str';
}

# Tagged export after `of`
{
    my subset Big of Int is export(:MATH) where * > 100;
    ok 200 ~~ Big, 'subset (of … is export(:tag) where) accepts';
    nok 50 ~~ Big, 'subset (of … is export(:tag) where) rejects';
}
