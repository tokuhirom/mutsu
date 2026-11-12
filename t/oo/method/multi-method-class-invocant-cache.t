use Test;

# Multi methods whose candidates differ by a `::?CLASS:D` / `::?CLASS:U`
# invocant (or take a `::?CLASS` argument) pick the right candidate on every
# call, for instances and type objects, in classes, subclasses and role
# consumers, whichever was called first.

plan 12;

class A {
    multi method w(::?CLASS:D:) { "D" }
    multi method w(::?CLASS:U:) { "U" }
    multi method same(::?CLASS $o) { "same" }
    multi method same($o) { "other" }
}
class B is A { }

is A.w, "U", 'type object first';
is A.new.w, "D", 'then an instance';
is A.w, "U", 'and the type object again';
is B.new.w, "D", 'a subclass instance';
is B.w, "U", 'a subclass type object';

is A.new.same(A.new), "same", '::?CLASS argument: same class';
is A.new.same(42), "other", '::?CLASS argument: other value';
is A.new.same(B.new), "same", '::?CLASS argument: subclass instance';

role R {
    multi method r(::?CLASS:D:) { "rD " ~ self.^name }
    multi method r(::?CLASS:U:) { "rU " ~ self.^name }
}
class C does R { }
class D does R { }
is C.new.r, "rD C", 'role multi on an instance';
is C.r, "rU C", 'role multi on a type object';
is D.new.r, "rD D", 'another consumer, instance';
is D.r, "rU D", 'another consumer, type object';
