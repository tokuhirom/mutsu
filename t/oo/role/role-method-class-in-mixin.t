use Test;

# `::?CLASS` in a role method is the class the role is composed into. For a
# role mixed in with `.^mixin` (what upstream NativeCall's
# `CArray.^parameterize` does), that is the mixin type, not the role
# (#11203: `TypedCArray!allocate` reads `::?CLASS.^array_type`).

plan 4;

role RR[::T] is array_type(T) {
    method cls()  { ::?CLASS.^name }
    method elem() { ::?CLASS.^array_type.^name }
}
class K is repr('CArray') is array_type(Str) { }

my \M = K.^mixin(RR[Str]);
is M.cls, 'K+{RR[Str]}', 'on a mixin type object, ::?CLASS is the mixin type';
is M.elem, 'Str', 'and its .^array_type is the role-supplied one';

role Plain { method cls() { ::?CLASS.^name } }
class C { }
is (C.new but Plain).cls, 'C+{Plain}', 'on a mixed-in instance too';
class D does Plain { }
is D.new.cls, 'D', 'a role composed at declaration still sees its class';
