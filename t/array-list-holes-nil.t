use v6;
use Test;

# `.List` materializes autovivification holes as `Nil`, unlike the Array gist
# which shows the `(Any)` type object. (raku-doc/doc/Type/Array.rakudoc)

plan 8;

my @a = [1];
@a[3] = 3;
is @a.List.raku, '(1, Nil, Nil, 3)', 'untyped autoviv holes become Nil in .List';
is @a.gist, '[1 (Any) (Any) 3]', 'Array gist still shows (Any) for holes';

my Int @b = [0];
@b[3] = 3;
is @b.List.raku, '(0, Nil, Nil, 3)', 'typed-array holes also become Nil in .List';

my @c;
@c[2] = 'x';
is @c.List.raku, '(Nil, Nil, "x")', 'leading holes become Nil';

# Bulk / literal arrays have no holes and are untouched.
my @d = 1, 2, 3;
is @d.List.raku, '(1, 2, 3)', 'bulk array .List is unchanged';
is [<a b c>].List.raku, '("a", "b", "c")', 'literal array .List is unchanged';

# An explicitly-assigned type object is NOT a hole (it is in the initialized set).
my @e;
@e[0] = Int;
@e[2] = 5;
is @e.List.raku, '(Int, Nil, 5)', 'assigned type object stays, gap becomes Nil';

# No autoviv: a fully-populated element-wise array has no Nil.
my @f;
@f[0] = 10; @f[1] = 20;
is @f.List.raku, '(10, 20)', 'contiguous element assignment has no holes';
