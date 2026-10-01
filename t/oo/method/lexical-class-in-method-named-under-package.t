use Test;

plan 6;

# A `my class` declared in a method body is named under the enclosing class,
# even when the class has nothing else that anchors the method's package.
class Q {
    method b { my class X { method h { self.^name } }; X.^name ~ "|" ~ X.new.h ~ "|" ~ X.raku }
    method c { { my class Y {}; Y.^name } }
    submethod d { my role R {}; R.^name }
}
is Q.b, "Q::X|Q::X|Q::X", 'my class in a method is named Q::X';
is Q.new.c, "Q::Y", 'my class in a nested block of a method';
is Q.d, "Q::R", 'my role in a submethod';

# A sub body is not a package: no prefix.
sub f { my class Z {}; Z.^name }
is f, "Z", 'my class in a plain sub keeps the bare name';

# A class that already anchored its package behaves the same.
class W { sub helper { 1 }; method m { my class V {}; V.^name } }
is W.m, "W::V", 'class with class-scoped sub';
class U { method m { my class T {}; T.^name } }
is U.m, "U::T", 'class with a single method';
