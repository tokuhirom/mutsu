use Test;

# Container identity (§3.1): a typed array reassigned as a whole THROUGH a shared
# `ContainerRef` cell (a `:=` bind alias) must keep its `array[T]` identity. The
# alias name carries no declared `var_type_constraint`, so the write-through path
# has to inherit the element type from the cell's current value; before the fix
# `@a = ...` through the bound cell downgraded `array[int]` to a plain `Array`.

plan 6;

{
    my int @a = 1, 2, 3;
    my @b := @a;
    is @a.WHAT.gist, '(array[int])', 'typed array starts as array[int]';
    @a = 4, 5, 6;
    is @a.WHAT.gist, '(array[int])', 'array[int] survives whole reassign via bound cell';
    is @b.WHAT.gist, '(array[int])', 'bound alias also reports array[int]';
    is @b[1], 5, 'bound alias sees the reassigned contents';
}

# The element type still guards writes through the bound cell.
{
    my int @a = 1, 2, 3;
    my @b := @a;
    dies-ok { @a = 'not an int' }, 'element type check still applies via bound cell';
}

# Untyped bound array is unaffected (stays a plain Array).
{
    my @a = 1, 2, 3;
    my @b := @a;
    @a = 'x', 'y';
    is @b, ('x', 'y'), 'untyped bound array reassigns normally';
}
