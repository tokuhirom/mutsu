use Test;

# A typed array keeps its declared element type across in-place mutating
# methods (pop/shift/splice/append/prepend/unshift) and across reassignment
# into the same container. Previously these demoted `array[int]` to `Array`
# because the pointer-keyed type metadata was dropped when `Arc::make_mut`
# reallocated the backing buffer.

plan 17;

# Native typed arrays — in-place mutators preserve the type
{
    my int @a = 1..5;
    @a.pop;
    is @a.WHAT.^name, 'array[int]', 'pop preserves array[int]';
}
{
    my int @a = 1..5;
    @a.shift;
    is @a.WHAT.^name, 'array[int]', 'shift preserves array[int]';
}
{
    my int @a = 1..5;
    @a.splice(0, 1);
    is @a.WHAT.^name, 'array[int]', 'splice preserves array[int]';
}
{
    my int @a = 1..5;
    @a.append(9);
    is @a.WHAT.^name, 'array[int]', 'append preserves array[int]';
}
{
    my int @a = 1..5;
    @a.prepend(9);
    is @a.WHAT.^name, 'array[int]', 'prepend preserves array[int]';
}
{
    my int @a = 1..5;
    @a.unshift(9);
    is @a.WHAT.^name, 'array[int]', 'unshift preserves array[int]';
}

# Other native widths
{
    my int8 @a = 1..5;
    @a.splice(0, 1);
    is @a.WHAT.^name, 'array[int8]', 'splice preserves array[int8]';
}

# Parameterized (non-native) typed arrays
{
    my Int @a = 1..5;
    @a.splice(0, 1);
    is @a.WHAT.^name, 'Array[Int]', 'splice preserves Array[Int]';
}
{
    my Int @a = 1..5;
    @a.pop;
    is @a.WHAT.^name, 'Array[Int]', 'pop preserves Array[Int]';
}

# splice returns the removed elements as the *same* typed container
{
    my int @a = 1..10;
    my \removed = @a.splice(8, 2);
    is removed.WHAT.^name, 'array[int]', 'splice return is array[int]';
    is removed.list, (9, 10), 'splice return holds the removed elements';
}
{
    my Int @a = 1..10;
    my \removed = @a.splice(0, 3);
    is removed.WHAT.^name, 'Array[Int]', 'splice return is Array[Int]';
}

# Reassigning into a `my`-declared typed container preserves its type
# (the name-keyed constraint already drives this).
{
    my int @a = 1..5;
    @a = (10, 20, 30);
    is @a.WHAT.^name, 'array[int]', '= into array[int] keeps the type';
}

# Mutators still produce correct values (not just types)
{
    my int @a = 1..5;
    is @a.pop, 5, 'pop returns the last element';
    is @a.list, (1, 2, 3, 4), 'pop removed the right element';
}
{
    my int @a = 1..5;
    my @r = @a.splice(1, 2);
    is @r.list, (2, 3), 'splice removed the right elements';
    is @a.list, (1, 4, 5), 'splice left the right remainder';
}

# vim: expandtab shiftwidth=4
