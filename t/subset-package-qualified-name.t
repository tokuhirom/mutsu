use Test;

plan 21;

# A `subset` declared in a package body belongs to that package, so its name
# is `Package::Name` -- that is what `.^name` reports and what every type-check
# message prints.

class Foo {
    subset RM of Str where * eq 'GET';
    has RM $.method is rw;
    method set($m) { $!method = $m }
    method set-acc($m) { $.method = $m }
    method inside-decl() { my RM $v = 'GET'; $v }
    method inside-match($x) { $x ~~ RM }
}

is Foo::RM.^name, 'Foo::RM', 'subset in a class body is named Package::Name';
is Foo.new(method => 'GET').method, 'GET', 'a conforming value passes .new';
is Foo.new.set('GET'), 'GET', 'a conforming value passes $!attr assignment';
is Foo.inside-decl, 'GET', 'the short name still resolves inside the package';
ok Foo.inside-match('GET'), 'smartmatch by the short name inside the package';
nok Foo.inside-match('PUT'), 'and it still rejects a non-conforming value';
ok 'GET' ~~ Foo::RM, 'smartmatch by the qualified name from outside';
nok 'PUT' ~~ Foo::RM, 'and the qualified name rejects too';

# The qualified name is what the type-check error reports.
throws-like { Foo.new.set('PUT') },
    /'expected Foo::RM but got Str'/,
    'the $!attr assignment error names the qualified subset';

throws-like { Foo.new.set-acc('PUT') },
    /'expected Foo::RM'/,
    'the $.attr assignment error names the qualified subset';

throws-like { Foo.new(method => 'PUT') },
    /'expected Foo::RM'/,
    'the .new type-check error names the qualified subset';

# `module` bodies qualify the same way.
module M {
    our subset Small of Int where * < 10;
}
is M::Small.^name, 'M::Small', 'subset in a module body is named Module::Name';
ok 5 ~~ M::Small, 'the module-qualified subset accepts';
nok 50 ~~ M::Small, 'the module-qualified subset rejects';

# A `my subset` is lexical: it gets no package-qualified alias.
class Bar {
    my subset Priv of Int where * > 0;
    method f($x) { $x ~~ Priv }
}
ok Bar.f(3), 'a `my subset` still matches inside its scope';
nok Bar.f(-3), 'a `my subset` still rejects inside its scope';

# A file-scope subset keeps its bare name.
subset Top of Int where * > 100;
is Top.^name, 'Top', 'a mainline subset is not qualified';
ok 200 ~~ Top, 'the mainline subset accepts';

# An inherited attribute reports the subset qualified by its DECLARING class.
class Base {
    subset Pos of Int where * > 0;
    has Pos $.n is rw;
    method bump($v) { $!n = $v }
}
class Derived is Base { }
is Derived.new(n => 5).n, 5, 'an inherited typed attribute accepts';
throws-like { Derived.new.bump(-1) },
    /'expected Base::Pos'/,
    'an inherited attribute names the subset by its declaring class';

# A nested class used as an attribute type still resolves (no regression).
class Outer {
    class Inner { has $.v }
    has Inner $.inner is rw;
}
is Outer.new(inner => Outer::Inner.new(v => 7)).inner.v, 7,
    'a nested-class attribute type still resolves';
