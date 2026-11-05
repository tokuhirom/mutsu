use Test;

# An element store into a typed array whose element type is a plain builtin
# (`Int`, `Str`, `Num`, `Bool`) and whose value is exactly that type is served
# by the compiled element-store lane (#14702). A value the constraint rejects,
# every other element type (a subtype, a coercion type, a native type, a role)
# and a typed array passed to an untyped parameter keep the full path's rules.
# Checked against rakudo.
plan 14;

{
    my Int @a;
    @a[$_] = $_ * 3 for ^4;
    is @a.join(','), '0,3,6,9', 'appending Ints to a typed Int array';
    @a[1] = 100;
    is @a[1], 100, 'an in-range store into a typed Int array';
    throws-like { @a[2] = "x" }, X::TypeCheck::Assignment, 'a Str into an Int array is rejected';
    throws-like { @a[4] = 1.5 }, X::TypeCheck::Assignment, 'a Rat into an Int array is rejected on an append';
    is @a.elems, 4, 'a rejected store leaves the array alone';
}

{
    my Str @s;
    @s[0] = 'a';
    @s[1] = 'b';
    is @s.join(','), 'a,b', 'a typed Str array';
    throws-like { @s[2] = 5 }, X::TypeCheck::Assignment, 'an Int into a Str array is rejected';
}

{
    my Num @n;
    @n[0] = 1e0;
    throws-like { @n[1] = 3 }, X::TypeCheck::Assignment, 'an Int into a Num array is rejected (no numeric widening)';
    my Bool @b;
    @b[0] = True;
    @b[1] = False;
    is @b.map(*.Str).join(','), 'True,False', 'a typed Bool array';
}

{
    my Numeric @m;
    @m[0] = 1;
    @m[1] = 2.5;
    is @m.join(','), '1,2.5', 'a supertype element type still takes its subtypes';
    subset Small of Int where * < 10;
    my Small @sm;
    @sm[0] = 5;
    throws-like { @sm[1] = 50 }, X::TypeCheck::Assignment, 'a subset element type still checks its where clause';
}

{
    my int @native;
    @native[0] = 7;
    is @native.join(','), '7', 'a native int array';
}

sub store-into(@x) { @x[@x.elems] = 'z'; @x }
{
    my Int @a = 1, 2;
    throws-like { store-into(@a) }, X::TypeCheck::Assignment, 'a typed array passed to an untyped parameter keeps its constraint';
}

{
    my Int @a is default(9);
    @a[0] = 1;
    @a[2] = 3;
    is @a[1], 9, 'a typed array with a default keeps it for a gap';
}
