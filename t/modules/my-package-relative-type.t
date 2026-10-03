use Test;

plan 3;

# A package-qualified type name in a `my` declaration resolves relative to
# the enclosing package, as it does in a signature.
class Outer {
    class Globber {
        class Match { has $.v }
        method prev() {
            my Globber::Match $prev = Globber::Match.new(:v(3));
            $prev.v
        }
        method untyped() {
            my Globber::Match $none;
            $none.^name
        }
    }
}

is Outer::Globber.prev, 3, 'my Pkg::Type $x accepts a value of the nested type';
is Outer::Globber.untyped, 'Outer::Globber::Match', 'its default is the nested type object';
throws-like { my Outer::Globber::Match $x = 42 }, X::TypeCheck::Assignment,
    'the fully qualified constraint still type-checks';
