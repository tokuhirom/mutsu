use Test;

# A parenthesized WhateverCode used as an operand of a further operator/method
# composes into a new, larger WhateverCode (Raku currying semantics).

plan 17;

# Infix with a WhateverCode operand
{
    my $f = (* - 1) - 1;
    isa-ok $f, Callable, '(* - 1) - 1 is a WhateverCode';
    is $f(6), 4, '(* - 1) - 1 composes';
}
{
    my $f = 1 +< (* - 1);
    is $f(6), 32, '1 +< (* - 1) composes';
}
{
    my $f = 1 +< (*-1) - 1;
    is $f(64), 9223372036854775807, 'chained infix over WhateverCode composes';
}
{
    my $f = (* + 1) * 2;
    is $f(6), 14, '(* + 1) * 2 composes';
}
{
    my $f = 2 * (* + 1);
    is $f(6), 14, '2 * (* + 1) composes';
}
{
    my $f = (* - 1) ** 2;
    is $f(4), 9, '(* - 1) ** 2 composes';
}

# Each bare * stays a separate positional argument when inlined
{
    my $f = * + (* - 1);
    is $f(10, 20), 29, 'outer * plus a parenthesized WhateverCode -> 2-arg';
}
{
    my $f = (* - 1) + (* - 1);
    is $f(10, 20), 28, 'two parenthesized WhateverCodes -> 2-arg';
}
{
    my $f = (* + *) - 1;
    is $f(10, 20), 29, 'parenthesized 2-arg WhateverCode composes';
}

# Method call on a WhateverCode target composes
{
    my $f = (* - 1).abs;
    is $f(-3), 4, '(* - 1).abs composes';
}
{
    my $f = (^*).roll;
    ok 0 <= $f(6) < 6, '(^*).roll composes into a rolling WhateverCode';
}
{
    my $f = -(* - 1);
    is $f(5), -4, 'prefix negate over a WhateverCode composes';
}

# `x` (string repeat) composes; `xx` does NOT (it replicates as a Seq value)
{
    my $f = (* - 1) x 3;
    is $f(6), '555', '(* - 1) x 3 composes (string repeat)';
}
{
    my @s = (* - 1) xx 3;
    is @s.elems, 3, '(* - 1) xx 3 is a 3-element Seq, not a WhateverCode';
    ok @s[0] ~~ Callable, 'each xx element is itself a WhateverCode';
}

# A direct call of a parenthesized WhateverCode is NOT composed
{
    is (* - 1)(5), 4, '(* - 1)(5) is a direct call, not a composition';
}

# vim: expandtab shiftwidth=4
