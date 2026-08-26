use Test;

plan 3;

# A `my class` nested in another class body must have its `trusts` honored.
# The nested class registers under a mangled lexical storage name
# (`Outer::Inner\0<decl-id>`, ADR-0047 P1), and by the time `Outer`'s method
# runs, the bare name `Inner` is no longer bound in the env the private-call
# permission check consults -- so the owner written in `$o!Inner::secret` has
# to be canonicalized against the invocant's own MRO instead.
#
# This file is deliberately separate from `t/metamodel-introspection.t`: a
# `trusts` declaration inside a nested class body makes every OTHER class in
# the same compilation unit report a package-qualified `.^name`
# (`Plain` -> `Outer::Plain`), a pre-existing bug tracked in
# `todo/tickets/nested-trusts-decl-qualifies-sibling-class-names.md`. Keeping
# these cases in their own file means that leak cannot contaminate unrelated
# assertions.

class Outer {
    my class Inner {
        trusts Outer;
        method !secret() { 'from Inner' }
    }
    method poke() { Inner.new()!Inner::secret() }
}
is Outer.poke, 'from Inner', 'a nested lexical class honors its `trusts`';

class OuterOur {
    our class InnerOur {
        trusts OuterOur;
        method !secret() { 'from InnerOur' }
    }
    method poke() { InnerOur.new()!InnerOur::secret() }
}
is OuterOur.poke, 'from InnerOur', 'a nested `our` class honors its `trusts`';

# Trust is not blanket permission: without `trusts`, the same shape is refused.
dies-ok {
    EVAL 'class Nest {
              my class Shut { method !secret() { 1 } }
              method poke() { Shut.new()!Shut::secret() }
          };
          Nest.poke'
}, 'a nested lexical class without `trusts` still refuses an outer caller';

done-testing;
