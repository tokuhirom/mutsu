use Test;

plan 16;

# Track B T5: a typed container's element constraint survives celling — every
# cas write path enforces it with X::TypeCheck::Assignment, exactly like a
# plain element assignment (verified against raku).

# 3-arg array cas rejects a wrong-typed swap value...
my Int @a;
@a[0] = 5;
throws-like { cas(@a[0], 5, "s") }, X::TypeCheck::Assignment,
    '1-dim array cas rejects Str into Int array';
is @a[0], 5, 'rejected swap leaves the element untouched';

# ...even when the compare would fail (raku checks the value regardless).
throws-like { cas(@a[0], 999, "s") }, X::TypeCheck::Assignment,
    'swap value is type-checked even when the compare fails';
is @a[0], 5, 'compare-fail + type-fail leaves the element untouched';

# Non-coercing mismatch (Rat into Int array).
throws-like { cas(@a[0], 5, 2.5) }, X::TypeCheck::Assignment,
    'Rat into Int array is rejected';
is @a[0], 5, 'Rat swap left element untouched';

# Multidim cas (shaped: an un-shaped `my Int @m` rejects the intermediate
# Array that `@m[1;1]` would autovivify).
my Int @m[2;2];
@m[1;1] = 7;
throws-like { cas(@m[1;1], 7, "x") }, X::TypeCheck::Assignment,
    'multidim array cas rejects Str into Int array';
is @m[1;1], 7, 'rejected multidim swap leaves the element untouched';

# Code-form array cas (newly supported) enforces the constraint on the
# computed value...
my Int @c;
@c[0] = 1;
throws-like { cas(@c[0], -> $v { "bad" }) }, X::TypeCheck::Assignment,
    'code-form array cas rejects wrong-typed result';
is @c[0], 1, 'rejected code-form swap leaves the element untouched';

# ...and works for well-typed results, including multidim.
cas(@c[0], -> $v { $v + 10 });
is @c[0], 11, 'code-form array cas applies a well-typed result';
my @m2 = [1,2],[3,4];
cas(@m2[1;1], -> $v { $v + 100 });
is @m2[1;1], 104, 'code-form multidim array cas works';

# Hash code-form cas enforces the constraint too.
my Int %h;
%h<k> = 1;
throws-like { cas(%h<k>, -> $v { "bad" }) }, X::TypeCheck::Assignment,
    'code-form hash cas rejects wrong-typed result';
is %h<k>, 1, 'rejected hash code-form swap leaves the value untouched';

# atomicint arrays keep working (native element type accepts Int swaps).
my atomicint @n[2;2];
@n[1;1] = 0;
cas(@n[1;1], 0, 42);
is @n[1;1], 42, 'atomicint multidim cas still swaps';
cas(@n[1;1], -> int $v { $v + 1 });
is @n[1;1], 43, 'atomicint code-form cas still works';
