use Test;

# A parametric role's sigilless type parameter (`role R[\u]`) is a term, so a
# same-named `$u` parameter of the role is a different variable
# (ADR-12441, #13551).
plan 4;

role R[\u, $u] { method m { "{u}{$u}" } }
class C does R[1, 2] { }
is C.new.m, '12', 'a sigilless and a scalar role parameter of the same name';

role S[$u, \u] { method m { "{u}{$u}" } }
class D does S[1, 2] { }
is D.new.m, '21', 'the scalar parameter first';

role T[\v] { method m { v } }
class E does T[5] { }
is E.new.m, 5, 'a lone sigilless role parameter';

role P[::X, \x] { method m { X.^name ~ x } }
class F does P[Int, 7] { }
is F.new.m, 'Int7', 'next to a type capture';
