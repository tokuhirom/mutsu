use v6.e.PREVIEW;
use Test;

# ADR-11276 §9.149 (#13738): language revision 6.e adds methods over ones a
# shaped type answers from an ancestor under 6.d. `Complex.sign` is the unit
# complex number; `Int.pick`/`Int.roll` sample `^self`. Under 6.d
# (`revision-d-shape-method-rows.t`) the ancestor's methods answer.

plan 10;

is-approx (1+2i).sign.re, 0.4472135954999579, 'Complex.sign, real part';
is-approx (1+2i).sign.im, 0.8944271909999159, 'Complex.sign, imaginary part';
is (3+0i).sign, 1+0i, 'Complex.sign of a real value is a Complex';
is (0+0i).sign, 0+0i, 'Complex.sign of zero';

ok 5.pick ~~ ^5, 'Int.pick is a value of ^self';
is 5.pick(*).sort.List, (0, 1, 2, 3, 4), 'Int.pick(*) is every value of ^self';
is 5.pick(2).unique.elems, 2, 'Int.pick($n) picks distinct values';
is 3.roll(20).grep(* !~~ ^3).elems, 0, 'Int.roll stays in ^self';
is 3.roll(20).elems, 20, 'Int.roll($n) rolls n times';
is 5.pick(0).elems, 0, 'Int.pick(0) is empty';
