use Test;

plan 7;

# A type declared inside a class body is scoped to that body. It must resolve
# from the class's own methods, and it must NOT outlive the body -- leaving the
# short name bound in the enclosing scope clobbered a same-named file-scope
# class for the rest of the program.

class Foo { method who() { "outer-Foo" } }

class CBTS-Outer {
    my class Foo { method who() { "inner-Foo" } }
    method make() { Foo.new }
}

is CBTS-Outer.make.who, 'inner-Foo',
    'a class-body `my class` resolves inside its own class methods';
is Foo.new.who, 'outer-Foo',
    'the file-scope class of the same name survives the nested declaration';
is Foo.^name, 'Foo', 'and it is still the file-scope type';

# The same for a role declared in a class body -- the shape
# `unit class HTTP::UserAgent; role Connection { ... }` uses, where methods
# further down write `my Connection $conn`.
role Marker { method tag() { "outer-Marker" } }

class CBTS-Role {
    role Marker { method tag() { "inner-Marker" } }
    class Impl does Marker { }
    method make() { my Marker $m = Impl.new; $m }
}

is CBTS-Role.new.make.tag, 'inner-Marker',
    'a class-body role resolves as a type constraint in a method body';
is Marker.^name, 'Marker', 'the file-scope role of the same name survives';

# And for a subset, which Cro::HTTP::Request declares as `subset Method of Str`.
subset Tag of Str where *.chars > 2;

class CBTS-Subset {
    subset Tag of Str where /^ <[A..Z]>+ $/;
    has Tag $.t is rw;
}

my $r = CBTS-Subset.new;
lives-ok { $r.t = "GET" }, 'a class-body subset constrains its attribute';
dies-ok { $r.t = "get" }, 'and rejects a value its predicate refuses';

done-testing;
