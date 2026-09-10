use v6;
use Test;

plan 7;

# A declaration nested inside a package-like declarator is installed under its
# composed name, and the parser must recognise that spelling as a type name.
# Without it, `when X::Foo::Missing { ... }` is misread as an undeclared
# bareword gobbling the block (X::Syntax::BlockGobbled), so the file does not
# even parse.

package GLOBAL::X::PkgNest {
    class Missing is Exception { method message { "missing" } }
}

module X::ModNest {
    class Gone is Exception { method message { "gone" } }
}

class X::ClassNest {
    class Inner is Exception { method message { "inner" } }
}

package GLOBAL::X::Deep {
    package Nest {
        class Leaf is Exception { method message { "leaf" } }
    }
}

is X::PkgNest::Missing.^name, 'X::PkgNest::Missing', 'package-nested class composes its name';
is X::Deep::Nest::Leaf.^name, 'X::Deep::Nest::Leaf', 'GLOBAL:: is stripped from the composed name';

sub match-it($type) {
    my $got = 'none';
    try {
        $type.new.throw;
        CATCH {
            when X::PkgNest::Missing   { $got = 'pkg' }
            when X::ModNest::Gone      { $got = 'mod' }
            when X::ClassNest::Inner   { $got = 'class' }
            when X::Deep::Nest::Leaf   { $got = 'deep' }
            default                    { $got = 'default' }
        }
    }
    $got;
}

is match-it(X::PkgNest::Missing), 'pkg', 'when on a package-nested class';
is match-it(X::ModNest::Gone), 'mod', 'when on a module-nested class';
is match-it(X::ClassNest::Inner), 'class', 'when on a class-nested class';
is match-it(X::Deep::Nest::Leaf), 'deep', 'when on a doubly-nested class';

# The guard this relies on must still reject a genuinely undeclared name: a
# bareword before a block gobbles the block, so the `when` has no body.
throws-like 'given 1 { when X::NeverDeclared::Nope { 42 } }', X::Comp::Group,
    'an undeclared X:: bareword still gobbles the block';
