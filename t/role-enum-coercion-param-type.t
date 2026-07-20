use v6;
use Test;

plan 4;

# An enum type name is a valid parameter constraint (and coercion target) in a
# role method — `method m(EnumType $x)` and `method m(EnumType(Str) $x)`. The
# role method param-type validation only recognized classes/roles/built-ins, so a
# (qualified) enum used in a role method param was rejected as
# "Invalid typename 'EnumType(Str)' in parameter declaration". Seen in the
# SQL::Abstract dist (`Join::Type(Str) :$type` inside a role).

enum J::Type (:Inner<inner>, :Left<left>);

role Src {
    method plain(J::Type $t) { $t.key }
    method coerced(J::Type(Str) $t) { $t.key }
}
class C does Src {}

pass 'a role with an enum-typed / enum-coercion method param compiles';
is C.new.plain(J::Type::Inner), 'Inner', 'plain enum param binds an enum value';

# An unqualified enum in a role param works too.
enum Flag <On Off>;
role R2 { method f(Flag $x) { $x.Int } }
class D does R2 {}
is D.new.f(On), 0, 'unqualified enum param in a role binds';

# The class-method form (non-role) still works, unchanged.
class E { method g(J::Type $t) { $t.key } }
is E.g(J::Type::Left), 'Left', 'enum param in a plain class method still works';
