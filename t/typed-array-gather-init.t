use v6;
use Test;

# Regression: initializing a typed array from a `gather` block.
#
# A plain `gather { ... }` produces a lazy-list value (not an eager Array/Seq).
# The typed-array TypeCheck path only element-checked Array/Seq values, so a
# gather RHS fell through to the *scalar* type-check and blew up with
# "Type check failed in assignment to @a; expected Int, got Array".
# (This is what made `zef locate ...` fall through to the CLI usage banner:
#  Zef::Client.resolve builds `my Candidate @results = gather {...}`.)

plan 11;

# Boxed element type, plain gather -> reified and element-checked.
{
    my Int @a = gather { take 1; take 2; take 3 };
    is @a.elems, 3, 'my Int @a = gather {...}: element count';
    is @a.join(','), '1,2,3', 'my Int @a = gather {...}: values';
    ok @a[0] ~~ Int, 'my Int @a = gather {...}: elements are Int';
}

# Str element type.
{
    my Str @s = gather { take 'x'; take 'y' };
    is @s.join(','), 'x,y', 'my Str @s = gather {...} works';
}

# Native element type: a plain gather is NOT lazy, so it reifies (no X::Cannot::Lazy).
{
    my int @n = gather { take 3; take 4 };
    is @n.join(','), '3,4', 'my int @n = gather {...} reifies into a native array';
    is @n.elems, 2, 'my int @n = gather {...}: element count';
}

# Untyped array still works (control).
{
    my @m = gather { take 5 };
    is @m.join(','), '5', 'my @m = gather {...} (untyped) works';
}

# A gather whose taken value violates the element type must throw
# X::TypeCheck::Assignment naming the offending element, not "got Array".
{
    my $ex;
    { my Int @e = gather { take 1; take 'oops' }; CATCH { default { $ex = $_ } } };
    isa-ok $ex, X::TypeCheck::Assignment, 'element type mismatch in gather RHS throws X::TypeCheck::Assignment';
}

# lazy gather: boxed array stays lazy (elements accessible on demand).
{
    my Int @b = lazy gather { take 10; take 20 };
    is @b[0], 10, 'lazy gather into Int @b: first element';
    is @b[1], 20, 'lazy gather into Int @b: second element';
}

# lazy gather into a native array is rejected as X::Cannot::Lazy.
{
    my $ex;
    { my int @c = lazy gather { take 7 }; CATCH { default { $ex = $_ } } };
    isa-ok $ex, X::Cannot::Lazy, 'lazy gather into native array throws X::Cannot::Lazy';
}
