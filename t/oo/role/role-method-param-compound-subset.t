use Test;

# A `subset` declared under a COMPOUND name is a type like any other, and a role
# method may name it in a parameter. mutsu's role-method signature validator
# asked `is_resolvable_type`, which consulted classes, roles and enums but never
# the subset registry; its short-name fallback did consult subsets but is gated
# on the constraint being unqualified, so only the compound spelling fell
# through and was reported as `Invalid typename '...' in parameter declaration.`
#
# From `License::Software` (a dependency of `App::Mi6` / `Ddt` in the ecosystem
# parity ledger): `License/Software/Year.pm6` declares
# `subset License::Software::Year where UInt | MyDateish | YearRange;` and
# `unit role License::Software::Abstract`'s `multi method new` takes one.
# https://github.com/tokuhirom/mutsu/issues/7993

plan 7;

# The reduction, in one file: a sub and a class method already accepted this
# spelling, only the role method did not.
subset Lic::Year where UInt;

sub takes-year(Lic::Year $y) { $y }
is takes-year(2020), 2020, 'a sub parameter accepts a compound-name subset';

class Holder {
    method show(Lic::Year $y) { $y }
}
is Holder.show(1999), 1999, 'a class method parameter accepts a compound-name subset';

role Abstract {
    method year(Lic::Year $y) { $y }
}
class Concrete does Abstract { }
is Concrete.year(1066), 1066, 'a role method parameter accepts a compound-name subset';

# The subset's `where` must still be enforced through the role method, i.e. the
# name resolves to the real subset rather than being waved through.
dies-ok { Concrete.year(-1) }, 'the subset constraint still rejects a bad value';

# The same name decorated, since the validator strips the decoration before
# looking the base up.
role Smiley {
    method year(Lic::Year:D $y) { $y }
}
class SmileyC does Smiley { }
is SmileyC.year(7), 7, 'a compound-name subset carrying a `:D` smiley resolves';

role Coerced {
    method year(Lic::Year(Any) $y) { $y }
}
class CoercedC does Coerced { }
is CoercedC.year(8), 8, 'a compound-name subset used as a coercion target resolves';

# Guard: a genuinely undeclared compound name is still rejected, so the fix did
# not turn the validator into a rubber stamp.
throws-like 'role Bogus { method m(No::Such::Type $x) { $x } }; class BogusC does Bogus { }',
    X::Parameter::InvalidType,
    'an undeclared compound typename is still reported';
