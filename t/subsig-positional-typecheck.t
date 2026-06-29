use Test;

plan 6;

# A non-slurpy @-sigil parameter inside a parameter sub-signature requires a
# Positional value at its position. Binding a scalar there is X::TypeCheck::Binding.
throws-like 'sub foo(@array ($first, @rest)) { say @rest }; foo <1 2 3>;',
    X::TypeCheck::Binding, got => IntStr, expected => Positional,
    'scalar bound to a non-slurpy @ sub-signature param throws';

throws-like 'sub foo(@a ($x, @y)) { }; foo [1, 2, 3];',
    X::TypeCheck::Binding, expected => Positional,
    'plain Int bound to a non-slurpy @ sub-signature param throws';

# A slurpy *@rest collects the remaining elements — no error.
lives-ok { sub foo(@array ($first, *@rest)) { }; foo [1, 2, 3] },
    'slurpy *@rest in a sub-signature binds the remaining elements';

# A genuinely nested Positional binds fine.
{
    my ($x, @y);
    sub foo(@a ($first, @second)) { $x = $first; @y = @second }
    foo [1, (2, 3)];
    is $x, 1, 'nested destructure binds the scalar element';
    is-deeply @y.List, (2, 3), 'nested destructure binds the inner Positional';
}

# The undeclared-symbol check (used by EVAL/throws-like) must treat
# sub-signature names as declared, not flag them as X::Undeclared.
{
    sub foo(@array ($first, *@rest)) { @rest.elems }
    is foo([1, 2, 3]), 2, 'sub-signature inner names are in scope in the body';
}
