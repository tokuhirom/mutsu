use Test;
use lib 't/lib';
use MetaopUserInfix;
use MetaopUserInfix::Ops;

plan 3;

# The same `multi infix:<X>` arrives through an `EXPORT` hook and a direct
# `use`; operands no candidate accepts must fall through to the core operator.
is-deeply ((1, 2) X (3, 4)).list, ((1, 3), (1, 4), (2, 3), (2, 4)).list,
    'core X when no imported candidate accepts the operands';
is-deeply ((1, 2) Z (3, 4)).list, ((1, 3), (2, 4)).list,
    'core Z when no imported candidate accepts the operands';
pass 'no runaway recursion';
