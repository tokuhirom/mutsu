use Test;

plan 16;

# When `infix:<<>>`/`infix:<>>`/`infix:<<=>>`/`infix:<>=>` are called as a
# routine (`&infix:«<»(...)`, the shape `Test.rakumod`'s `cmp-ok` uses via
# `&CALLER::LEXICAL::("infix:<$op>")`) rather than compiled inline as an
# operator, the runtime fallback in `apply_reduction_op` (ops_reduction.rs)
# numified both operands through a `to_num`/`to_int` closure that had no case
# for `ValueView::BigInt`/`ValueView::BigRat` (the boxed representation used
# once a numerator/denominator overflows the inline i64 view). Both silently
# fell through to a `0` default, so any comparison against a big rational or
# big integer was compared as if it were exactly zero.

my $bigint = 10 ** 30;
is &infix:«<»($bigint, 5), False, 'big positive Int is not < a small Int';
is &infix:«>»($bigint, 5), True, 'big positive Int is > a small Int';
is &infix:«<=»($bigint, 5), False, 'big positive Int is not <= a small Int';
is &infix:«>=»($bigint, 5), True, 'big positive Int is >= a small Int';
is &infix:«<»(-$bigint, 5), True, 'big negative Int is < a small Int';
is &infix:«>»(5, -$bigint), True, 'a small Int is > a big negative Int';

my $bigrat = 10 ** 30 + 0.5;
is $bigrat.WHAT, Rat, 'sanity: 10**30 + 0.5 is a (big) Rat';
is &infix:«<»($bigrat, 5), False, 'big positive Rat is not < a small Int';
is &infix:«>»($bigrat, 5), True, 'big positive Rat is > a small Int';
is &infix:«<=»(-$bigrat, -5), True, 'big negative Rat is <= a small negative Int';
is &infix:«>=»(-5, -$bigrat), True, 'a small negative Int is >= a big negative Rat';

my $bigfatrat = (10 ** 30 + 0.5).FatRat;
is &infix:«<»($bigfatrat, 5), False, 'big positive FatRat is not < a small Int';
is &infix:«>»($bigfatrat, 5), True, 'big positive FatRat is > a small Int';
is &infix:«<=»(-$bigfatrat, -5), True, 'big negative FatRat is <= a small negative Int';
is &infix:«>=»(-5, -$bigfatrat), True, 'a small negative Int is >= a big negative FatRat';

# This is the exact shape that regressed under `MUTSU_REAL_TEST=1` in
# t/bigrat-sort-compare.t: cmp-ok calls the matcher as a routine.
cmp-ok (-2 ** 80 + 0.1).FatRat, '<', -0.5,
    'cmp-ok correctly compares a big negative FatRat against a small Rat';
