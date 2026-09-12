use Test;

# `has @.a[N]` where `N` is a named `constant` (not a literal integer) must
# still be recognized as a shaped-array declaration.
#
# `CompiledAttrDecl::declared_shape` (src/opcode.rs) is extracted statically
# from the compiler-generated `Array.new(:shape(N))` default, and that
# extraction only ever understood a literal `N` — so a constant-dimension
# shape came back `declared_shape: None`, indistinguishable from a plain
# unshaped `has @.a`. The *runtime* shape was always fine (`Array.new` itself
# evaluates `N` correctly), so `.shape` on a built instance reported right;
# only consumers reading the class-level `declared_shape` (constructor
# coercion of a provided value here, NativeCall's CStruct layout in
# nativecall-has-array-shape-constant.t) saw the gap (GH #8032).

plan 6;

constant N = 3;
class Row { has @.a[N]; }
is Row.new.a.shape, (3,), 'a named-constant single-dim shape is recognized on an uninitialized instance';

my $row = Row.new(a => [1, 2, 3]);
is $row.a.shape, (3,), 'a provided value is coerced into the named-constant shape';
is $row.a, [1, 2, 3], 'the provided values survive the coercion';

constant M = 2;
class Grid { has @.a[N, M]; }
is Grid.new.a.shape, (3, 2), 'a multi-dim shape mixing named constants is recognized';

# A literal dimension keeps working (no regression).
class Lit { has @.a[4]; }
is Lit.new.a.shape, (4,), 'a literal-dimension shape is still recognized';

# A role's shaped attribute is resolved at role-registration time and carries
# through composition.
role R { has @.a[N]; }
class FromRole does R { }
is FromRole.new.a.shape, (3,), 'a role attribute with a named-constant shape survives composition';
