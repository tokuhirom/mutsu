use Test;

plan 7;

# A type capture called with arguments coerces through the type it is bound
# to (`T($v)` in `role R[::T]` is `Int($v)` for `R[Int]`), in a role body and
# in a routine whose signature captures the type; `T()` with no arguments
# stays the coercion type `T(Any)` (#18939).

role Typed[::T] { method make($v) { T($v) } }
class IntBox does Typed[Int] {}
is IntBox.new.make("41") + 1, 42, 'a role type capture coerces';

role R3[::T] { method m($v) { my T $x = T($v); $x } }
is-deeply (class :: does R3[Str] {}).new.m(5), "5", 'into a variable typed by it';

role R4[::T] { method coerce { T() } }
is (class :: does R4[Int] {}).coerce.raku, 'Int(Any)', 'T() is the coercion type';

sub f(::T $x) { T("3") }
is f(5) + 1, 4, 'a routine type capture coerces';

sub g(::T $x, $y) { T($y) }
is-deeply g("a", 12), "12", '... into the type of another argument';

sub h(::T $x) { T.new }
is h(5), 0, 'a method call on the capture still works';

role R5[::T] { method m { T } }
is (class :: does R5[Rat] {}).m.^name, 'Rat', 'a bare capture is the type';
