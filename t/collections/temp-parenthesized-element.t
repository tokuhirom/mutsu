use Test;

# `temp` over an element whose container is parenthesized (`temp (@a)[0] = v`)
# or a further subscript with no assignment (`temp @n[0][1];`) temporizes that
# element, as rakudo does. `temp (` was read as a call of an unknown function
# `temp` (#10581), and the bare multi-level form was a parse error.

plan 12;

{
    my @e = 1, 2;
    { temp (@e)[0] = 99; is-deeply @e, [99, 2], 'temp (@a)[i] = v assigns' }
    is-deeply @e, [1, 2], 'temp (@a)[i] = v restores at scope exit';
}

{
    my @e = 1, 2;
    { temp (@e)[1]; @e[1] = 7; is-deeply @e, [1, 7], 'bare temp (@a)[i] keeps later writes' }
    is-deeply @e, [1, 2], 'bare temp (@a)[i] restores';
}

{
    my @n = [1, 2], 3;
    { temp ((@n)[0])[1] = 9; is-deeply @n, [[1, 9], 3], 'nested parens: assigns' }
    is-deeply @n, [[1, 2], 3], 'nested parens: restores';
}

{
    my @n = [1, 2], 3;
    { temp @n[0][0]; @n[0][0] = 8; is-deeply @n, [[8, 2], 3], 'bare multi-level temp parses' }
    is-deeply @n, [[1, 2], 3], 'bare multi-level temp restores';
}

{
    my %h = a => 1;
    { temp (%h)<a> = 5; is %h<a>, 5, 'temp (%h)<k> = v assigns' }
    is %h<a>, 1, 'temp (%h)<k> = v restores';
}

{
    my @e = 1, 2;
    { temp (@e)[0] = 42 if True; is-deeply @e, [42, 2], 'with a statement modifier' }
    is-deeply @e, [1, 2], 'with a statement modifier: restores';
}
