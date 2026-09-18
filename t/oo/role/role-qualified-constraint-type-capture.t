use v6;
use Test;

plan 4;

# WebDriver2 0.1.12 (ecosystem sweep): `role Resolvable[ Foo::Context ::T ] { }`
# — a parametric role whose type parameter pairs a NAMESPACED nominal
# constraint with a `::T` type capture, written with a leading space after
# the `[` (the common style) — failed to compose with "No matching candidate
# found for the parametric role" even though the applied argument satisfied
# the constraint.
#
# Root cause: `parse_optional_role_type_params` (src/parser/stmt/class/
# role_decl.rs) called `parse_param_list` on the bracket content without
# trimming the leading whitespace after `[`, so the leading-space form always
# fell through to a fallback parser. That fallback split the param text on
# the FIRST literal "::" to separate the constraint from the capture, which
# is wrong whenever the constraint itself is namespaced (`Foo::Context ::T`
# split at "Foo" / "Context ::T" instead of "Foo::Context" / "T"), silently
# dropping the type parameter and leaving the role with zero parameters —
# so any application with an argument failed arity matching.

role Ctx::Base { method top { ... } }
role Ctx::Sub does Ctx::Base { method id { ... } }

role Ctx::Base {
    method top { ... }
}
role Ctx::Sub does Ctx::Base {
    method id { ... }
}

role Ctx::Resolvable[ Ctx::Base ::T ] {
    method resolve( --> T:D ) { ... }
}

role Plain {
    method a { }
}

class Elem does Plain does Ctx::Resolvable[ Ctx::Sub ] {
    method resolve( --> Ctx::Sub:D ) { !!! }
    method a { }
    method top { }
    method id { }
}

ok Elem.new, 'class composing a role parameterized with [ NamespacedType ::T ] (leading space) composes';
ok Elem.new ~~ Ctx::Resolvable[Ctx::Sub], 'the composed instance does the parameterized role with its bound argument';

# No-leading-space spelling must keep working too.
role Ctx::Resolvable2[Ctx::Base ::U] {
    method resolve2( --> U:D ) { ... }
}
class Elem2 does Ctx::Resolvable2[Ctx::Sub] {
    method resolve2( --> Ctx::Sub:D ) { !!! }
}
ok Elem2.new, 'class composing a role parameterized with [NamespacedType ::T] (no leading space) composes';

# A bare (unqualified) constraint with leading space must also keep working.
role Based[::V] { method get-v { V } }
class Elem3 does Based[Int] { }
is Elem3.new.get-v, Int, 'bare-name constrained type capture role param still composes';
