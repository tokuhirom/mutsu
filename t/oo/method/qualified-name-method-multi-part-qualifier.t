use Test;

# A qualified method call whose qualifier is itself a multi-part package name
# (`.A::B::C::m`) splits at the LAST `::`: the method is `m`, the qualifier
# `A::B::C`. From the SVG distribution (via Math::Polygons), whose
# `class SVG is XML::Writer` calls `self.XML::Writer::serialize(...)` from a
# method that is invoked on the type object (`SVG.serialize(...)`).

plan 6;

class A::B::C {
    method m($x) { "C.m($x)" }
}
class D is A::B::C {
    method m($x) { "D.m -> " ~ self.A::B::C::m($x) }
}

is D.new.m(1), 'D.m -> C.m(1)', 'self.Multi::Part::method on an instance';
is D.new.A::B::C::m(2), 'C.m(2)', 'instance invocant, multi-part qualifier';
is D.m(3), 'D.m -> C.m(3)', 'self.Multi::Part::method on a type object';
is D.A::B::C::m(4), 'C.m(4)', 'type-object invocant, multi-part qualifier';
is A::B::C.A::B::C::m(5), 'C.m(5)', 'type object qualified with its own name';

class E { }
throws-like { E.A::B::C::m(6) }, X::Method::InvalidQualifier,
    method => 'm', 'non-ancestor multi-part qualifier names the method correctly';
