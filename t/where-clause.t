use Test;

plan 13;

# Block form
{
    sub check-positive(Int $x where { $_ > 0 }) { "positive" }
    is check-positive(5), "positive", "block form where clause accepts valid value";
    dies-ok { check-positive(-1) }, "block form where clause rejects invalid value";
}

# WhateverCode form
{
    sub check-even(Int $x where * %% 2) { "even" }
    is check-even(4), "even", "WhateverCode where clause accepts valid value";
    dies-ok { check-even(3) }, "WhateverCode where clause rejects invalid value";
}

# Smartmatch/literal form
{
    sub check-zero(Int $n where 0) { "zero" }
    is check-zero(0), "zero", "literal where clause accepts matching value";
    dies-ok { check-zero(1) }, "literal where clause rejects non-matching value";
}

# Multi dispatch with where clauses (the main bug fix)
{
    multi sub num-kind(Int $n where * > 0) { "positive" }
    multi sub num-kind(Int $n where * < 0) { "negative" }
    multi sub num-kind(Int $n where 0)     { "zero" }

    is num-kind(42),  "positive", "multi dispatch with where clause selects positive";
    is num-kind(-7),  "negative", "multi dispatch with where clause selects negative";
    is num-kind(0),   "zero",     "multi dispatch with where clause selects zero";
}

# Where clause without explicit type
{
    sub check-small($x where { $_ < 10 }) { "small" }
    is check-small(5), "small", "where clause without explicit type works";
    dies-ok { check-small(20) }, "where clause without type rejects invalid value";
}

# Multi with typed where + untyped fallback
{
    multi sub describe(Int $n where * > 0) { "positive int" }
    multi sub describe(Int $n)             { "other int" }

    is describe(5),  "positive int", "where-constrained multi chosen over unconstrained";
    is describe(-3), "other int",    "unconstrained multi chosen when where fails";
}
