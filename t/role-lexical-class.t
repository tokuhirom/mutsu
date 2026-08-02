use Test;

plan 8;

# A `my class` declared in a role body is private to the role, and the role's
# own methods must keep seeing it after they have been composed into a class --
# even when a same-named type exists at file scope or in another package.

class Foo { method who() { "outer-Foo" } }

class InClass {
    my class Bar { method who() { "inner-class-Bar" } }
    method make() { Bar.new }
}

role InRole {
    my class Foo { method who() { "inner-role-Foo" } }
    method make() { Foo.new }
}

class UsesRole does InRole { }

is InClass.make.who, 'inner-class-Bar',
    'a class-body `my class` resolves inside its own class methods';
is InClass.make.^name, 'InClass::Bar',
    'a class-body `my class` is registered under its class';

is UsesRole.new.make.who, 'inner-role-Foo',
    'a role-body `my class` resolves inside the composed role method';
is UsesRole.new.make.^name, 'InRole::Foo',
    'a role-body `my class` is registered under its role';

# The role-lexical type must not leak into unrelated scopes.
is Foo.new.who, 'outer-Foo', 'the outer class of the same name is unaffected';

# A role-lexical class that composes a role of its own still wins over the
# outer same-named class.
role R2 {
    my class Foo does Callable { method who() { "r2-Foo" } }
    method make-foo() { Foo.new }
}
class C2 does R2 { }

is C2.new.make-foo.^name, 'R2::Foo',
    'a role-lexical class composing a role keeps its own identity';
is C2.new.make-foo.who, 'r2-Foo',
    'the role-lexical class wins over the outer same-named class';

# A class that composes the role but declares its own method referring to the
# bare name still sees the file-scope class -- the role only lends its lexical
# types to methods that came from the role.
class C3 does R2 {
    method own() { Foo.new.who }
}
is C3.new.own, 'outer-Foo',
    'a consuming class own method still sees the file-scope class';

done-testing;
