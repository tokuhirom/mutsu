use Test;

# Shadow-slot allocation is DEFAULT ON since 2026-07-12: a shadowing inner-block
# `my $x` gets its own local slot, so mutating it never touches the outer binding
# and the outer value is intact after the block. This pins the default-on
# behaviour (was gated behind MUTSU_SHADOW_SLOTS; `MUTSU_NO_SHADOW_SLOTS` opts out).

plan 8;

{
    my $x = 1;
    { my $x = 2; $x++; is $x, 3, 'inner shadow mutates independently'; }
    is $x, 1, 'outer scalar intact after shadowed block';
}

{
    my @a = 1, 2, 3;
    { my @a = <x y>; push @a, 'z'; is @a.elems, 3, 'inner @-shadow independent'; }
    is @a.join(','), '1,2,3', 'outer array intact after shadowed block';
}

# Assigning to an OUTER (non-shadowed) variable inside a block DOES persist.
{
    my $outer = 10;
    { $outer = 42; }
    is $outer, 42, 'assignment to an outer var persists out of the block';
}

# Nested triple shadow, each level independent.
{
    my $v = 'a';
    {
        my $v = 'b';
        {
            my $v = 'c';
            is $v, 'c', 'depth-2 shadow';
        }
        is $v, 'b', 'depth-1 shadow intact';
    }
    is $v, 'a', 'depth-0 intact';
}
