use v6;
use Test;

# Found via SBOM::CycloneDX: a class's own multi method wins an otherwise equal
# tie against one composed from a role, whatever order they were registered in.

plan 4;

role R { multi method m(::?CLASS: :$raw-error = False, *%in) { "role" } }
class A does R { multi method m(A:U: :$raw-error) { "class" } }
is A.m, 'class', 'class candidate beats the role slurpy-named one';

role R2 { multi method m(::?CLASS: :$raw-error = False, *%in) { "role" } }
class B does R2 { multi method m(B: :$raw-error) { "class-plain" } }
is B.m, 'class-plain', 'tie on an undefined invocant';
is B.new.m, 'class-plain', 'tie on a defined invocant';

role R3 { multi method new(::?CLASS: :$raw-error = False, *%in) { "role-new" } }
class C does R3 { multi method new(C:U: :$raw-error) { "class-new " ~ %_.keys.sort } }
is C.new(:x(1)), 'class-new x', 'the %_ of the class candidate sees the named args';
