use Test;

plan 7;

# The rvalue of a list assignment is the LHS list (the assigned targets), not the
# whole RHS: `(($a, $b) = 1, 2, 3)` in list context is `($a, $b)`.

{
    my ($a, $b);
    my @z = (($a, $b) = 1, 2, 3);
    is @z.elems, 2, 'list-assignment rvalue has one element per scalar target';
    is ~@z, '1 2', '... the assigned values';
    is $a, 1, 'binding is correct ($a)';
    is $b, 2, 'binding is correct ($b)';
}

# More than two scalar targets.
{
    my ($p, $q, $r);
    my @z = (($p, $q, $r) = 10, 20, 30);
    is @z.elems, 3, 'three scalar targets => three-element rvalue';
    is ~@z, '10 20 30', '... the assigned values';
}

# The binding still slurps a trailing `@` target correctly.
{
    my ($a, @b);
    ($a, @b) = 1, 2, 3, 4;
    is ~@b, '2 3 4', 'a trailing slurpy @ target still binds the tail';
}
