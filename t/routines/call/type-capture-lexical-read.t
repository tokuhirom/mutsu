use Test;

# A type capture is a lexical of its routine: `T` inside `sub f(::T $x)`,
# or inside a method of `role R[::T]`, is the type the call bound, read
# directly (`GetTypeCapture`) rather than through the bareword resolution
# chain. From Monad's `method some($value) { some-type(..., T).new(...) }`,
# whose `T` read cost a full bareword resolution on every call.
plan 9;

class T { }

sub f(::T $x) { T.^name }
is f(42), 'Int', 'a signature capture wins over a same-named class';

role R[::T = Str] {
    method m() { T.^name }
    method c() { my &b = { T.^name }; b() }
}
is R.m, 'Str', 'role type parameter at its default on the role type object';
is R[Int].m, 'Int', 'role type parameter of a parameterization';
is R[Rat].c, 'Rat', 'role type parameter read from a block in the method';

class C does R[Num] { }
is C.new.m, 'Num', 'role type parameter of a composed role';

sub g(::U $x, ::V $y) { U.^name ~ ',' ~ V.^name }
is g(1, 'a'), 'Int,Str', 'two captures of one signature';

sub outer(::W $x) { my &inner = { W.^name }; inner() }
is outer(1.5e0), 'Num', 'an enclosing capture read from a closure';

sub rec(::X $x, $n) { $n == 0 ?? X.^name !! rec('s', $n - 1) ~ '/' ~ X.^name }
is rec(1, 2), 'Str/Str/Int', 'each recursive call reads its own capture';

my @seen = (1, 'a', 2.5).map: -> $v { f($v) };
is @seen.join(','), 'Int,Str,Rat', 'repeated calls rebind the capture';
