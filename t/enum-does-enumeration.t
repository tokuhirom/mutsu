use v6;
use Test;

# Every enum type object and every enum value does the `Enumeration` role:
# `E ~~ Enumeration`, `E.pick ~~ Enumeration`, `.does(Enumeration)`, and a
# `Enumeration $x` parameter all recognize enums. Previously `Enumeration` was
# not even a known type (it stringified), so all of these were False / errors.

plan 15;

enum E <a b c>;

# Bareword resolves to a type object, not a string.
is Enumeration.^name, 'Enumeration', 'Enumeration is a type object';

# Smartmatch: enum type object and enum value.
ok (E ~~ Enumeration),  'enum type object ~~ Enumeration';
ok (a ~~ Enumeration),  'enum value ~~ Enumeration';
ok (b ~~ Enumeration),  'another enum value ~~ Enumeration';

# Non-enums do NOT match.
nok (5 ~~ Enumeration),      'Int does not ~~ Enumeration';
nok ('x' ~~ Enumeration),    'Str does not ~~ Enumeration';
class Plain {}
nok (Plain ~~ Enumeration),  'a plain class does not ~~ Enumeration';

# Definiteness smileys: a value is defined (:D), a type object is undefined (:U).
ok  (a ~~ Enumeration:D),  'enum value ~~ Enumeration:D';
nok (a ~~ Enumeration:U),  'enum value does not ~~ Enumeration:U';
ok  (E ~~ Enumeration:U),  'enum type object ~~ Enumeration:U';

# `.does`.
ok a.does(Enumeration), 'enum value .does(Enumeration)';
ok E.does(Enumeration), 'enum type object .does(Enumeration)';

# A `Enumeration` parameter binds an enum value.
{
    sub takes-enum(Enumeration $x) { $x.key }
    is takes-enum(a), 'a', 'Enumeration parameter binds an enum value';
}

# Works in a smartmatching context (grep / when).
{
    my @mixed = a, 5, b, 'x', c;
    is @mixed.grep(Enumeration).elems, 3, 'grep(Enumeration) keeps the enum values';
}

# A non-Int-backed enum value still does Enumeration.
{
    enum Frac(:x(1.5), :y(2.5));
    ok (x ~~ Enumeration), 'Rat-backed enum value ~~ Enumeration';
}

done-testing;
