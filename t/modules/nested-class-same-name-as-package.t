use Test;

# From the Marrow distribution (API::Db): `unit class Foo::Bar; class Foo::Bar { }`.
# The nested class takes the written name over; its own name is package-qualified.
plan 4;

class Foo::Bar {
    class Foo::Bar {
        has $.x;
        method hi { "hi {$.x}" }
    }
    method outer { "outer" }
}

is Foo::Bar.^name, 'Foo::Bar::Foo::Bar', 'written name names the nested class';
is Foo::Bar.new(x => 3).hi, 'hi 3', 'nested class methods are reachable';
is Foo::Bar.new.can('outer').elems, 0, 'outer class is shadowed';

class C { class C { method hi { 'inner' } }; method outer { 'outer' } }
is C.new.can('outer').elems, 1, 'a simple name nested in itself does not take over';
