use v6;
use Test;

# A placeholder variable inside a parenthesised sub-expression must still be
# collected as a block parameter. `^($^x)` desugars to `0 ..^ Grouped($^x)`,
# and the placeholder scan skipped the `Grouped` node — so the block took no
# parameters and `$^x` read as an unbound default.
# (raku-doc Language/structures.rakudoc)
{
    my &a-func = { (^($^þ)).Seq };
    is a-func(3), (0, 1, 2),         'unicode placeholder in parens under prefix-^ binds';
    is a-func(7), (0, 1, 2, 3, 4, 5, 6), 'and again with a different argument';
}

{
    my &f = { ^($^x) };
    is f(3).raku, '^3', 'ascii placeholder in parens binds';
}

# Multiple placeholders inside parens, and nested parens.
{
    my &g = { ($^a) + ($^b) };
    is g(3, 4), 7, 'two parenthesised placeholders bind in order';

    my &h = { (($^x)) };
    is h(9), 9, 'doubly-parenthesised placeholder binds';
}

# Unparenthesised placeholders are unaffected.
{
    my &p = { $^x + 1 };
    is p(5), 6, 'bare placeholder still binds';
    my &q = { ^$^x };
    is q(3).raku, '^3', 'prefix-^ on a bare placeholder still binds';
}

done-testing;
