use Test;

# A package-scoped enum answers to its declared name (`.^name` is `E`, not
# `M::E`), and so does the type argument in a typed container's name.
# From Math::NIntegrate's RangeEndCases (#16913).

plan 12;

module M {
    our enum E <a b>;
    our class C { }
}
import M;

is M::E.^name, 'E', 'an enum in a package has its declared name';
is M::C.^name, 'M::C', 'a class keeps its qualified name';

my M::E @ea = M::E::a;
is @ea.WHAT.raku, 'Array[E]', 'typed array of an enum: .WHAT';
is @ea.WHAT.^name, 'Array[E]', 'typed array of an enum: .^name';
is @ea.raku, 'Array[E].new(E::a)', 'typed array of an enum: .raku';

my M::C @ca;
is @ca.WHAT.raku, 'Array[M::C]', 'typed array of a class stays qualified';

my M::E %eh = x => M::E::b;
is %eh.WHAT.raku, 'Hash[E]', 'typed hash of an enum: .WHAT';
is %eh.raku, '(my E % = :x(E::b))', 'typed hash of an enum: .raku';

my M::C %ch;
is %ch.WHAT.raku, 'Hash[M::C]', 'typed hash of a class stays qualified';

is Array[M::E].^name, 'Array[E]', 'parameterizing Array with an enum';
is Hash[M::E].^name, 'Hash[E]', 'parameterizing Hash with an enum';
is Array[M::C].^name, 'Array[M::C]', 'parameterizing Array with a class';
