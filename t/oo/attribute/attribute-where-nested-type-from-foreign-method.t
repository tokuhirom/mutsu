use Test;

# From the Fortran::Grammar suite (via IO::Glob::Globber): an attribute `where`
# predicate names a role declared next to its class. It must resolve that name
# in the declaring class even when the object is built from a method of an
# unrelated class.

plan 3;

class Outer {
    class Globber {
        role Term { }
        class Mt does Term { has $.v }
        has @.terms where { .elems > 0 && all($_) ~~ Term };
    }
    method build-ok { Globber.new(terms => [Globber::Mt.new(v => 1)]).terms.elems }
    method build-bad { Globber.new(terms => [1]) }
}

class Other {
    method build { Outer::Globber.new(terms => [Outer::Globber::Mt.new(v => 2)]).terms.elems }
}

is Outer.new.build-ok, 1, 'where predicate sees the nested role from the owning class';
is Other.new.build, 1, '... and from a method of an unrelated class';
dies-ok { Outer.new.build-bad }, 'a non-Term still fails the constraint';
