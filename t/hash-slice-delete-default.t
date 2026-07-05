use Test;

# `:delete` of an ABSENT key on a hash with `is default(X)` yields X, not Nil —
# for a SLICE delete too, where each absent key in the result list is filled
# with the default (single-key delete already did this).

plan 6;

{
    my %h is default(42) = a => 1, b => 2;
    is-deeply %h<a b c>:delete, (1, 2, 42), 'slice delete fills absent key with default';
}
{
    my %h is default(42) = a => 1, b => 2;
    is %h<c>:delete, 42, 'single-key delete of an absent key yields the default';
    is-deeply %h<x y>:delete, (42, 42), 'slice delete of all-absent keys is all default';
}
{
    # No `is default`: absent keys are the type object (Any), not a default.
    my %h = a => 1;
    is-deeply %h<a z>:delete, (1, Any), 'without a default, absent keys are Any';
}
{
    # The delete still removes the present keys.
    my %h is default(0) = a => 1, b => 2, c => 3;
    %h<a c q>:delete;
    is-deeply %h, {b => 2}, 'present keys are actually deleted';
}
{
    my %h is default('x') = one => 1;
    is-deeply %h<one two>:delete, (1, 'x'), 'string default in a slice delete';
}
