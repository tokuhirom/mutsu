use Test;

# `Rat * Rat` and `Rat * Int` take a fast path, and an i64-sized Rat result
# is reduced without BigInts (#10111). Every answer must be the general
# path's: reduced, sign on the numerator, a BigRat for a large numerator,
# Num (or FatRat under `$*RAT-OVERFLOW`) for a denominator past uint64, and
# a user `infix:<*>` still wins. A `Rat`/`FatRat`/`Int`/`Complex` parameter
# accepts its own type and rejects the others.

plan 17;

is-deeply (3/4) * 4, 3.0, 'Rat * Int';
is-deeply 4 * (3/4), 3.0, 'Int * Rat';
is-deeply (2/3) * (9/4), 1.5, 'Rat * Rat reduces';
is-deeply (-2/3) * (3/-4), 0.5, 'signs normalize';
is-deeply (1/3) * -3, -1.0, 'a negative Int';
is-deeply (5/7) * 0, 0.0, 'times zero';
my ($q, $n) = 3/4, -4;
is-deeply ($q * $n, $n * $q, $q * $q), (-3.0, -3.0, 0.5625), 'through variables';
is ((1/3) * 9223372036854775807).nude, (9223372036854775807, 3), 'a numerator past i64 stays exact';
isa-ok (1/4294967295) * (1/4294967296), Rat, 'a denominator within uint64 stays a Rat';
# Through variables: a literal product is folded at compile time, outside
# any `$*RAT-OVERFLOW` scope.
my ($tiny-a, $tiny-b) = 1/9223372036854775807, 1/9223372036854775806;
isa-ok $tiny-a * $tiny-b, Num, 'a denominator past uint64 is a Num';
{
    my $*RAT-OVERFLOW = FatRat;
    isa-ok $tiny-a * $tiny-b, FatRat, 'or a FatRat under $*RAT-OVERFLOW';
}
isa-ok FatRat.new(1, 3) * (3/4), FatRat, 'a FatRat operand keeps FatRat';

{
    sub infix:<*>(Rat $a, Int $b) { 'user' }
    my $r = 3/4;
    is $r * 4, 'user', 'a user infix:<*> takes the call';
}

sub takes-rat(Rat $r) { $r }
sub takes-int(Int $i) { $i }
is takes-rat(3/4), 0.75, 'a Rat parameter accepts a Rat';
throws-like { takes-rat(FatRat.new(1, 2)) }, X::TypeCheck::Binding::Parameter,
    'a Rat parameter rejects a FatRat';
is takes-int(2**70), 2**70, 'an Int parameter accepts a BigInt';
multi kind(Rat $) { 'Rat' }
multi kind(FatRat $) { 'FatRat' }
multi kind(Complex $) { 'Complex' }
multi kind(Int $) { 'Int' }
is-deeply (1/2, FatRat.new(1, 2), 1+2i, 2**70, 3).map({ kind($_) }).List,
    <Rat FatRat Complex Int Int>.List, 'multi candidates by numeric type';
