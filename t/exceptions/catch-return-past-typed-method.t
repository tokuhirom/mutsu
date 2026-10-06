use Test;
use MONKEY-SEE-NO-EVAL;

# A `return` in a CATCH block leaves the routine that wrote the CATCH. The
# exception is thrown deeper, in a method with a return type, so the signal
# crosses that method's call boundary on its way out. The boundary declines it
# (it names another routine) -- and used to hand it to its own `--> T` check
# anyway, which reported "expected Str but got Any (X::Foo())" for a `return`
# the method never executed (#11938, Template::HAML).

plan 15;

class X::Foo is Exception { method message { "foo" } }
class Node { }

class H {
    method plain(--> Str)           { X::Foo.new.throw }
    method untyped()                { X::Foo.new.throw }
    method int(--> Int)             { X::Foo.new.throw }
    method nil(--> Nil)             { X::Foo.new.throw }
    method forty-two(--> 42)        { X::Foo.new.throw }
    submethod sub-m(--> Str)        { X::Foo.new.throw }
    method !priv(--> Str)           { X::Foo.new.throw }
    method call-priv                { self!priv }
    multi method mm(H:U: :$src! --> Node) { X::Foo.new.throw }
    multi method mm(H:D: :$src! --> Node) { X::Foo.new.throw }
    method nested(--> Str)          { self.plain }
}

sub trap(&body) {
    try { body(); CATCH { when X::Foo { return $_ } } }
    Nil
}

# The shape from Template::HAML's t/0480: the routine with the CATCH has no
# return type of its own, and returns the exception.
my $h = H.new;
isa-ok trap({ $h.plain }), X::Foo, 'typed method --> Str';
isa-ok trap({ $h.untyped }), X::Foo, 'untyped method (already worked)';
isa-ok trap({ $h.int }), X::Foo, 'typed method --> Int';
isa-ok trap({ $h.nil }), X::Foo, 'definite --> Nil';
isa-ok trap({ $h.forty-two }), X::Foo, 'definite --> 42';
isa-ok trap({ $h.sub-m }), X::Foo, 'submethod with a return type';
isa-ok trap({ $h.call-priv }), X::Foo, 'private method with a return type';
isa-ok trap({ H.mm(:src<x>) }), X::Foo, 'multi method on the type object --> Node';
isa-ok trap({ $h.mm(:src<x>) }), X::Foo, 'multi method on an instance --> Node';
isa-ok trap({ $h.nested }), X::Foo, 'typed method called from a typed method';
isa-ok trap({ EVAL '$h.plain' }), X::Foo, 'typed method thrown from EVAL code';

# The shape from Template::HAML's Lint.rakumod: the CATCH is in a bare block of
# a method whose own return type is unrelated to the callee's.
class Linter {
    method lint(:$src! --> List) {
        my $tree;
        {
            CATCH { when X::Foo { return (:rule<parse-error>,).List } }
            $tree = H.mm(:src($src));
        }
        (:rule<ok>,).List
    }
    method lint-bad(:$src! --> List) {
        {
            CATCH { when X::Foo { return "not a list" } }
            H.mm(:src($src));
        }
        (:rule<ok>,).List
    }
}
is-deeply Linter.new.lint(:src<x>), (:rule<parse-error>,).List,
    'CATCH in a bare block returns from the enclosing --> List method';

# The method's own return type still applies to the `return` it is given.
throws-like { Linter.new.lint-bad(:src<x>) }, X::TypeCheck::Return,
    'the returning method still checks its own --> type';

# An explicit `return` inside the typed method itself is unaffected.
class Own {
    method ok(--> Str)  { return "fine" }
    method bad(--> Int) { return "nope" }
}
is Own.new.ok, 'fine', 'a typed method returns its own value';
throws-like { Own.new.bad }, X::TypeCheck::Return,
    'a typed method still checks its own return value';
