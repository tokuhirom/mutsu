use Test;

plan 9;

# An omitted optional parameter of a native `num` / `str` type binds the type's
# zero (`0e0` / `""`), as `int` already did, not the type's name (roast
# MISC/misc.t, #16412).
sub f(num $v?)   { $v }
sub g(num32 $v?) { $v }
sub h(str $v?)   { $v }
sub k(int $v?)   { $v }

is-deeply f(), 0e0, 'sub: num';
is-deeply g(), 0e0, 'sub: num32';
is-deeply h(), '',  'sub: str';
is-deeply k(), 0,   'sub: int (unchanged)';
is-deeply (-> num $v? { $v })(), 0e0, 'pointy block';
is-deeply sub (num $v?) { $v }(), 0e0, 'anonymous sub';

my class C {
    method m(num $v?)    { $v }
    submethod s(num $v?) { $v }
}
is-deeply C.new.m, 0e0, 'method';
is-deeply C.new.s, 0e0, 'submethod';

# A supplied value and a declared default are untouched.
sub d(num $v = 5e0) { $v }
is-deeply d(), 5e0, 'declared default';
