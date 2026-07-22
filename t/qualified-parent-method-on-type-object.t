use v6;
use Test;

# A qualified method call on a *type object* (`Foo.Bar::baz`) dispatches to the
# method defined in the qualifier class, not the most-derived override.
# Regression: mutsu reported X::Method::InvalidQualifier because a type object
# reports its meta-type ("Package") for the inheritance check.

plan 8;

class Bar { method baz { 42 } }
class Foo is Bar { method baz { "nope" } }

is Foo.Bar::baz, 42,     'type object: qualifier picks the parent method';
is Foo.Foo::baz, "nope", 'type object: own qualifier picks the override';
is Foo.new.Bar::baz, 42, 'instance: qualifier picks the parent method';

class Empty is Bar { }
is Empty.Bar::baz, 42, 'inherited (non-overridden) parent method via type object';

is (-42).Int::abs, 42, 'builtin qualified method still works';

role R { method greet { "hi" } }
class C does R { }
is C.R::greet, "hi", 'role-qualified method on a type object';

# A qualifier that is neither the type nor an ancestor/role is an error.
throws-like 'class A {}; class B {}; B.A::foo', X::Method::InvalidQualifier,
    'unrelated qualifier throws X::Method::InvalidQualifier';

# Deeper hierarchy.
class Grand { method g { "grand" } }
class Mid is Grand { method g { "mid" } }
class Leaf is Mid { method g { "leaf" } }
is Leaf.Grand::g, "grand", 'qualifier reaches a grandparent method';
