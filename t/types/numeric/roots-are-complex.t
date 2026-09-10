use Test;

# `Numeric.roots` mirrors Rakudo's
#   (^$n).map: { Complex.new-from-polar($mag ** (1/$n), ($angle + $_*2*pi)/$n) }
# guarded by three scalar early returns. Every assertion below was verified
# against Rakudo, and this file passes under `raku` unmodified.

plan 26;

# --- Every element of the list form is a Complex -----------------------------
# The regression this pins: collapsing a root whose imaginary part is a
# rounding-error epsilon back to a Num made `4.roots(2)` answer `(2e0, -2e0)`.

is 4.roots(2).elems, 2, '4.roots(2) has two roots';
is 4.roots(2)[0].WHAT.^name, 'Complex', 'first root of 4 is a Complex';
is 4.roots(2)[1].WHAT.^name, 'Complex', 'second root of 4 is a Complex';
is 4.roots(2).Str, '2+0i -2+2.4492935982947064e-16i', '4.roots(2) renders as Rakudo does';

is 4.roots(3).elems, 3, '4.roots(3) has three roots';
ok 4.roots(3).all ~~ Complex, 'every cube root of 4 is a Complex';

is 1.roots(4)[0].WHAT.^name, 'Complex', 'a root that is exactly real is still a Complex';
is 1.roots(4).Str,
   '1+0i 6.123233995736766e-17+1i -1+1.2246467991473532e-16i -1.8369701987210297e-16-1i',
   '1.roots(4) renders as Rakudo does';

# Zero is not special-cased: the general polar formula produces the -0 real part.
is 0.roots(2).Str, '0+0i -0+0i', '0.roots(2) keeps the negative zero of the general formula';

# --- n < 1 answers a bare NaN, not a one-element list ------------------------

ok 4.roots(0) ~~ NaN, '4.roots(0) is NaN';
is 4.roots(0).WHAT.^name, 'Num', '4.roots(0) is a bare Num, not a list';
ok 4.roots(-1) ~~ NaN, '4.roots(-1) is NaN';
is 4.roots(-1).WHAT.^name, 'Num', '4.roots(-1) is a bare Num, not a list';

# --- n == 1 answers the receiver as a Complex --------------------------------

is 4.roots(1).WHAT.^name, 'Complex', '4.roots(1) is a Complex';
is 4.roots(1).Str, '4+0i', '4.roots(1) is the receiver coerced to Complex';
is (2+3i).roots(1).Str, '2+3i', 'a Complex receiver comes back unchanged';
is Inf.roots(1).Str, 'Inf+0i', 'Inf.roots(1) is Inf+0i';
is NaN.roots(1).Str, 'NaN+0i', 'NaN.roots(1) is NaN+0i';

# --- a non-finite polar magnitude answers a bare NaN -------------------------

ok Inf.roots(2) ~~ NaN, 'Inf.roots(2) is NaN';
is Inf.roots(2).WHAT.^name, 'Num', 'Inf.roots(2) is a bare Num';
ok NaN.roots(2) ~~ NaN, 'NaN.roots(2) is NaN';
ok (Inf+1i).roots(2) ~~ NaN, 'a Complex with an infinite component is NaN';
ok (NaN+1i).roots(2) ~~ NaN, 'a Complex with a NaN component is NaN';

# --- Complex.Str renders its components exactly as Num.Str -------------------
# Rakudo's Complex.Str is `$!re ~ sign ~ $!im.abs ~ 'i'`, so the
# scientific-notation threshold is Num.Str's and cannot drift from it.

is (6.123233995736766e-17 + 1i).Str, '6.123233995736766e-17+1i',
   'a tiny real part uses scientific notation, as `say 6.1e-17` does';
is Complex.new(1e16, 2).Str, '1e+16+2i', 'a large integral real part uses scientific notation';
is Complex.new(1, 1e20).Str, '1+1e+20i', 'a large imaginary part uses scientific notation';
