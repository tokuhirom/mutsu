use Test;

plan 3;

# `.HOW` on an enum VALUE (`Solid`, an instance of `Fuel`), not just on the
# enum TYPE object (`Fuel`), must report `Perl6::Metamodel::EnumHOW`. Enum
# values have their own dedicated `Value` representation, distinct from a
# plain class `Instance`; `dispatch_how`'s type-name resolution had no case
# for it and fell through to a catch-all that reported `Mu` (hence
# `Perl6::Metamodel::ClassHOW`) for every enum value's `.HOW`, even though the
# type object's own `.HOW` already resolved correctly. Distributions that
# tell an enum value apart from a plain `Int` via `$value.HOW ~~
# Metamodel::EnumHOW` (ASN::BER's `Serializer`/`Parser`) silently picked the
# wrong multi candidate as a result.

enum Fuel <Solid Liquid Gas>;

ok Solid.HOW ~~ Metamodel::EnumHOW, 'an enum VALUE reports EnumHOW from .HOW';
ok Fuel.HOW ~~ Metamodel::EnumHOW, 'the enum TYPE object still reports EnumHOW too';
nok Solid.HOW ~~ Metamodel::ClassHOW, 'an enum value is not reported as an ordinary ClassHOW instance';

# vim: expandtab shiftwidth=4
