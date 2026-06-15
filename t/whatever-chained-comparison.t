use Test;

# Whatever-currying of chained comparisons: `a OP m OP b` curries over its
# distinct operands. The chain expands internally to `(a OP m) && (m OP b)` with
# the middle `m` duplicated; the WhateverCode arity must count `m`'s placeholders
# once (so a single-`*` middle gives a 1-arg WhateverCode, not 2-arg).

plan 12;

# --- single placeholder in the middle term ---
{
    my $f = (1 < *+1 < 5);
    isa-ok $f, Code, '1 < *+1 < 5 curries (1 arg)';
    is-deeply $f(3), True,  '... (3) -> True';
    is-deeply $f(5), False, '... (5) -> False';
    is-deeply $f(0), False, '... (0) -> False';
}

# --- method call in the middle term ---
{
    my $f = (11 < *.flip < 50);
    isa-ok $f, Code, '11 < *.flip < 50 curries';
    is-deeply $f(52),   True,  '... (52) -> True';
    is-deeply $f(25),   False, '... (25) -> False';
    is-deeply $f("01"), False, '... ("01") -> False';
}

# --- multiple operands, each contributing its own placeholders ---
{
    # operands: `22 + *.flip` (1), `*² + *.flip²` (2), `*` (1) => arity 4
    my $f = (22 + *.flip < *² + *.flip² < *);
    isa-ok $f, Code, '3-way chain with compound operands curries (4 args)';
    is-deeply $f(23, 45, 67, 100000), True,  '... True case';
    is-deeply $f(23, 45, 67, 10),     False, '... False case';
}

# --- plain (non-chained) curry still 2-arg ---
is (* + *)(40, 2), 42, 'non-chained `* + *` stays 2-arg';
