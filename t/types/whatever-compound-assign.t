use Test;

plan 11;

# An OP= right operand is part of the assignment metaoperator, not a plain
# assignment value. This pins ADR-0033's priming rule for the source-preserving
# CompoundAssign marker as well as the compiler's execution expansion.
{
    my $out = '';
    my $append = $out ~= *;

    isa-ok $append, WhateverCode, '`~= *` makes a WhateverCode';
    is $append.WHAT.gist, '(WhateverCode)', 'the callback has the right type object';
    $append('a');
    $append('b');
    is $out, 'ab', 'the curried compound assignment writes through its outer lexical';
}

{
    my $sum = 0;
    my $add = $sum += *;

    isa-ok $add, WhateverCode, '`+= *` also makes a WhateverCode';
    $add(2);
    $add(3);
    is $sum, 5, 'the numeric compound callback writes through its outer lexical';
}

{
    my $out = '';
    my $append = $out ~= *;
    is $append('x'), 'x', 'the callback returns the compound-assignment value';
}

{
    my $sum = 0;
    my $add-one = $sum += * + 1;

    isa-ok $add-one, WhateverCode, 'a compound right expression also curries';
    is $add-one(2), 3, 'the compound right expression receives the callback value';
    is $sum, 3, 'the compound right expression writes through its outer lexical';
}

{
    my @values = 1, 2;
    my $add = @values[0] += *;

    isa-ok $add, WhateverCode, 'a subscript compound assignment curries';
    $add(3);
    is @values[0], 4, 'the subscript callback preserves its existing writeback path';
}
