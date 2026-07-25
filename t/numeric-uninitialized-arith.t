use v6;
use Test;

# A bare concrete-numeric type object used as an operand of an *arithmetic*
# infix (`+ - * / % **`) throws X::Numeric::Uninitialized, just like the
# comparison ops (t/numeric-uninitialized-infix.t).
#
# The assignment metaop must keep working: rakudo's METAOP_ASSIGN never applies
# the base infix to an undefined container -- it substitutes the operator's
# zero-argument value first (0 for `+`/`-`, 1 for `*`/`**`) and throws
# X::NoZeroArgMeaning for the operators that have none (`/`, `%`). mutsu emits
# that substitution as its own opcode on the LHS, which is what lets the bare
# infix stay strict.

plan 37;

# --- Bare arithmetic infix throws -------------------------------------------
throws-like { my $x = Int + 1 }, X::Numeric::Uninitialized, 'Int + 1 throws';
throws-like { my $x = Int - 1 }, X::Numeric::Uninitialized, 'Int - 1 throws';
throws-like { my $x = Int * 2 }, X::Numeric::Uninitialized, 'Int * 2 throws';
throws-like { my $x = Int / 2 }, X::Numeric::Uninitialized, 'Int / 2 throws';
throws-like { my $x = Int ** 2 }, X::Numeric::Uninitialized, 'Int ** 2 throws';
{
    my $t = Int;
    throws-like { my $x = $t % 2 }, X::Numeric::Uninitialized, 'Int % 2 throws';
}
throws-like { my $x = 1 + Int }, X::Numeric::Uninitialized, 'type object on the right throws';

# --- Other concrete numeric types -------------------------------------------
throws-like { my $x = Num + 1 }, X::Numeric::Uninitialized, 'Num + 1 throws';
throws-like { my $x = Rat + 1 }, X::Numeric::Uninitialized, 'Rat + 1 throws';
throws-like { my $x = FatRat + 1 }, X::Numeric::Uninitialized, 'FatRat + 1 throws';
throws-like { my $x = Real + 1 }, X::Numeric::Uninitialized, 'Real + 1 throws';
throws-like { my $x = Bool + 1 }, X::Numeric::Uninitialized, 'Bool + 1 throws';

# --- Non-concrete-numeric type objects still warn+coerce, they do not throw --
is (quietly Any + 1), 1, 'Any + 1 coerces to 0 and does not throw';
is (quietly Str + 1), 1, 'Str + 1 coerces to 0 and does not throw';
is (quietly Numeric + 1), 1, 'Numeric + 1 coerces to 0 and does not throw';

# --- METAOP_ASSIGN seeds the zero-argument value ----------------------------
{
    my Int $a;
    $a += 1;
    is $a, 1, '+= on a type object seeds 0';
}
{
    my Int $a;
    $a -= 3;
    is $a, -3, '-= on a type object seeds 0';
}
{
    my Int $a;
    $a *= 5;
    is $a, 5, '*= on a type object seeds 1';
}
{
    my Int $a;
    $a **= 2;
    is $a, 1, '**= on a type object seeds 1';
}
{
    my Rat $a;
    $a += 0.1;
    is $a, 0.1, '+= on a Rat type object keeps the Rat result';
    isa-ok $a, Rat, 'the seeded 0 does not force the result to Int';
}
{
    my Num $a;
    $a += 1.5e0;
    is $a, 1.5e0, '+= on a Num type object seeds 0';
}
{
    my $a;
    $a += 1;
    is $a, 1, '+= on an untyped (Any) container still works';
}
{
    my $a = Nil;
    $a += 5;
    is $a, 5, 'Nil resets the container, so += seeds 0';
}

# --- Operators with no zero-argument meaning throw --------------------------
throws-like { my Int $a; $a /= 2 }, X::NoZeroArgMeaning, '/= on a type object throws';
throws-like { my Int $a; $a %= 2 }, X::NoZeroArgMeaning, '%= on a type object throws';
{
    my $ex;
    { my Int $a; $a /= 2; CATCH { default { $ex = $_ } } }
    is $ex.message, 'No zero-argument meaning for: infix:</>', 'message matches rakudo';
    is $ex.name, 'infix:</>', '.name is the offending operator';
}

# --- Only the LHS is seeded: a type object on the RHS still throws ----------
throws-like { my $a = 1; $a += Int }, X::Numeric::Uninitialized,
    'a type object RHS is not covered by the metaop identity';

# --- A Failure is concrete, so it is NOT replaced by the identity -----------
throws-like { my $a = Failure.new('boom'); $a *= 5 }, Exception,
    '*= propagates a Failure instead of seeding 1';

# --- Defined values are unaffected ------------------------------------------
{
    my $sum = 0;
    $sum += $_ for 1 .. 10;
    is $sum, 55, 'ordinary += loop is unaffected';
}
{
    my Int $n = 3;
    $n *= 4;
    $n -= 2;
    is $n, 10, 'compound assignment on a defined container is unaffected';
}
is 2 + 3, 5, 'ordinary addition is unaffected';
is 7 % 3, 1, 'ordinary modulo is unaffected';

# --- Element / attribute targets --------------------------------------------
{
    my Int @a;
    @a[0] += 5;
    is @a[0], 5, '+= on an uninitialized typed array element seeds 0';
}
{
    my Int %h;
    %h<k> *= 6;
    is %h<k>, 6, '*= on an absent typed hash key seeds 1';
}
{
    class C { has Int $.n is rw }
    my $c = C.new;
    $c.n += 7;
    is $c.n, 7, '+= on an uninitialized typed attribute seeds 0';
}
