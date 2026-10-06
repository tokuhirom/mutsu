use Test;

# A method closes over a sigilless binding of the routine that declares its
# class or role: a bare `t` in `sub mk(\t) { role { method of { t } } }` is the
# parameter, not the bareword string "t". Regression (#11804): the method body
# was compiled with no knowledge of the enclosing sigilless names, so `t` read
# as `Str` (the type of "t") instead of the argument; a `$t` capture worked.

plan 14;

# --- the issue's repro: an anonymous role built per call ----------------------
sub mk(Mu \t) { role { method of { t } } }
is (Any.new but mk(Int)).of.^name, 'Int', 'an anonymous role method reads a sigilless param (Int)';
is (Any.new but mk(Str)).of.^name, 'Str', 'each call closes over its own argument (Str)';
sub mk2($t) { role { method of { $t } } }
is (Any.new but mk2(Int)).of.^name, 'Int', 'the $-sigiled form keeps working';

# --- the argument is a value, not only a type object --------------------------
sub mk3(\t) { role { method of { t } } }
is (Any.new but mk3(42)).of, 42, 'a role method reads a sigilless Int argument';
is (Any.new but mk3("abc")).of, 'abc', 'a role method reads a sigilless Str argument';

# --- the same shape in a class declared inside the routine ---------------------
sub mk4(\t) { my class C { method of { t } }; C.new }
is mk4(42).of, 42, 'a class method reads a sigilless param of the enclosing sub';

# --- a plain closure always worked; it stays the reference ---------------------
sub mk5(\t) { sub { t } }
is mk5(42)(), 42, 'a closure reads the sigilless param';

# --- a sigilless `my \x` of the enclosing routine ------------------------------
sub mk6 {
    my \x = 99;
    my role R { method m { x } }
    (class :: does R { }).new.m
}
is mk6(), 99, 'a method reads a `my \x` of the enclosing routine';

# --- nested routines: both levels are visible ----------------------------------
sub outer(\a) {
    sub inner(\b) { my class D { method m { a + b } }; D.new.m }
    inner(2)
}
is outer(40), 42, 'a method sees the sigilless params of every enclosing routine';

# --- the method's own sigilless param shadows the enclosing one ----------------
sub shadow(\t) { my class E { method m(\t) { t } }; E.new.m(7) }
is shadow(1), 7, "the method's own \\t wins over the enclosing \\t";

# --- a mainline sigilless term is still readable from a top-level class --------
my \top = 5;
class T { method m { top } }
is T.new.m, 5, 'a mainline `my \top` is readable from a class method';

# --- the meta-method shape from the issue ---------------------------------------
class Box {
    method ^parameterize(Mu \c, Mu \t) { c.^mixin(role { method of { t } }) }
}
is Box[Int].of.^name, 'Int', 'a ^parameterize that mixes in a role reads its sigilless param';

# --- it is a lexical read, not a bareword lookup: nothing named `t` leaks -------
sub plain-bareword { my class F { method m { try { EVAL 'nonesuch-bareword' } // 'undeclared' } }; F.new.m }
is plain-bareword(), 'undeclared', 'an unrelated bareword in a method stays undeclared';
sub not-captured(\u) { my class G { method m { 'no capture needed' } }; G.new.m }
is not-captured(1), 'no capture needed', 'a method that never names the param is unaffected';
