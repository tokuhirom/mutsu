use Test;
plan 2;

# Test that grammar inside a class/module correctly inherits from
# the builtin Grammar, not a package-scoped Grammar.

class Foo {
    grammar G {
        token TOP { 'hello' }
    }
    method test() { G.parse('hello') }
}

ok Foo.new.test(), 'grammar inside class inherits from builtin Grammar';

# Grammar inside a nested class
class Bar {
    class Baz {
        grammar G2 {
            token TOP { 'world' }
        }
    }
}

ok Bar::Baz::G2.parse('world'), 'grammar inside nested class works';
