use Test;

plan 9;

# A bare type object inherits Mu's `.Numeric`/`.Real`: it warns
# "Use of uninitialized value of type T in numeric context" and returns 0,
# rather than throwing X::Method::NotFound.

is-deeply (quietly Mu.Numeric), 0, 'Mu.Numeric is 0';
is-deeply (quietly Mu.Real), 0, 'Mu.Real is 0';

my class Bare {}
is-deeply (quietly Bare.Numeric), 0, 'user-class type object .Numeric is 0';
is-deeply (quietly Bare.Real), 0, 'user-class type object .Real is 0';

# The coercion warns about an uninitialized value in numeric context.
warns-like { Mu.Numeric }, *.contains('uninitialized' & 'numeric'),
    'Mu.Numeric warns about uninitialized value in numeric context';

# Concrete numeric type objects still give their typed zero (fast path).
is-deeply (quietly Int.Numeric), 0, 'Int.Numeric is 0';
is-deeply (quietly Num.Numeric), 0e0, 'Num.Numeric is 0e0';
is-deeply (quietly Rat.Numeric), 0.0, 'Rat.Numeric is 0.0';

# A user-defined .Numeric still takes priority over the Mu default.
my class HasNum { method Numeric { 99 } }
is HasNum.Numeric, 99, 'user .Numeric method wins over the Mu default';
