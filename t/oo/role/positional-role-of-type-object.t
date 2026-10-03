use Test;

# `.of` answers the type argument of a composed `Positional[T]` /
# `Associative[V]`, on a type object as well as an instance, and through a
# parametric role that passes its own parameter on (#11726; upstream
# NativeCall's `CArray[uint8].of`).

plan 7;

class A does Positional[Int] { }
is A.of.^name, 'Int', 'type object composing Positional[Int]';
is A.new.of.^name, 'Int', 'instance composing Positional[Int]';

role R[::T] does Positional[T] { }
class B does R[Str] { }
is B.of.^name, 'Str', 'through a parametric role passing its parameter on';

class Sub-B is B { }
is Sub-B.of.^name, 'Str', 'inherited from a parent class';

role H[::V] does Associative[V] { }
class D does H[Rat] { }
is D.of.^name, 'Rat', 'Associative through a parametric role';

class C { }
my \M = C.^mixin(R[Num]);
is M.of.^name, 'Num', 'a .^mixin type object';

role Fixed does Positional[Bool] { }
class E does Fixed { }
is E.of.^name, 'Bool', 'a plain role that does Positional[Bool]';
