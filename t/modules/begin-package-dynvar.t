use v6;
use Test;

plan 3;

# `$*PACKAGE` is a dynamic variable that should resolve to the package
# currently being compiled while a BEGIN/CHECK phaser inside a class body
# is running -- the same compile-time phase that lets `has`-declarations
# from a compile-time EVAL attach to the enclosing class (#8790). Outside
# that phase it stays unbound, same as rakudo.
{
    my $seen;
    class Baz {
        BEGIN { $seen = $*PACKAGE.^name }
    }
    is $seen, 'Baz', '$*PACKAGE resolves to the enclosing class inside BEGIN';
}

# A nested class body's BEGIN sees the innermost package, and the binding is
# restored to the outer one once the inner class body's phaser has run.
{
    my ($inner, $outer);
    class Outer {
        class Inner {
            BEGIN { $inner = $*PACKAGE.^name }
        }
        BEGIN { $outer = $*PACKAGE.^name }
    }
    is $inner, 'Outer::Inner', 'nested class BEGIN sees the inner package';
    is $outer, 'Outer', 'outer class BEGIN still sees its own package after the inner one runs';
}
