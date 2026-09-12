use Test;

# A user subclass of a builtin metamodel class is the documented way to hook
# type creation, and `new_type` is the metamethod it hooks. Three gaps found
# while making the Test::Async distribution load, all reduced from
# `Test::Async::Metamodel::BundleHOW`
# (`unit class ... is Metamodel::ParametricRoleHOW;` with a `new_type(|)`
# override whose body is `callsame`):
#
# 1. `Metamodel::ParametricRoleHOW` was not on the list of builtin types a
#    user class may name as a parent, so the declaration itself died with
#    "cannot inherit from 'Metamodel::ParametricRoleHOW' because it is
#    unknown" — even though mutsu exposes the type and its `new_type` works.
# 2. `callsame` from such a `new_type` override answered `Nil`: the native
#    metamethod was offered as the base candidate only while an EXPORTHOW
#    DECLARE was in flight, so a plain `MyHOW.new_type(...)` had no candidate
#    at all.
# 3. `new_type` on a subclass that does NOT override it died with "No such
#    method 'new_type'" — the native metamethod was reachable only on the
#    `Metamodel::*` type objects themselves.

plan 12;

class PlainRoleHOW is Metamodel::ParametricRoleHOW { }
class PlainClassHOW is Metamodel::ClassHOW { }

# (1) + (3): inherited, non-overridden `new_type`.
my \r = PlainRoleHOW.new_type(name => 'MopHowRole');
is r.^name, 'MopHowRole', 'inherited new_type mints the named type (role HOW)';
is r.HOW.^name, 'PlainRoleHOW', 'and the new type carries the user HOW';

my \c = PlainClassHOW.new_type(name => 'MopHowClass');
is c.^name, 'MopHowClass', 'inherited new_type mints the named type (class HOW)';
is c.HOW.^name, 'PlainClassHOW', 'and the new type carries the user HOW';

# (2): an override that delegates to the native metamethod with `callsame`.
my @seen;
class HookingRoleHOW is Metamodel::ParametricRoleHOW {
    method new_type(|) {
        my \type = callsame;
        @seen.push: type.^name;
        type
    }
}

my \h = HookingRoleHOW.new_type(name => 'MopHowHooked');
is h.^name, 'MopHowHooked', 'callsame in a new_type override answers the minted type';
is-deeply @seen, ['MopHowHooked'], 'the override saw the type, not Nil';
is h.HOW.^name, 'HookingRoleHOW', 'the overriding HOW is installed too';

# `nextsame` reaches the same base candidate.
class TailRoleHOW is Metamodel::ParametricRoleHOW {
    method new_type(|) { nextsame }
}
is TailRoleHOW.new_type(name => 'MopHowTail').^name, 'MopHowTail',
    'nextsame in a new_type override reaches the native metamethod';

# The builtin metaclasses themselves are unaffected.
is Metamodel::ParametricRoleHOW.new_type(name => 'MopHowNative').^name, 'MopHowNative',
    'the builtin role metaclass still mints types';
is Metamodel::ParametricRoleHOW.new_type(name => 'MopHowNativeHow').HOW.^name,
    'Perl6::Metamodel::ParametricRoleHOW',
    'and such a type reports the builtin metaclass';
is Metamodel::ClassHOW.new_type(name => 'MopHowNativeClass').HOW.^name,
    'Perl6::Metamodel::ClassHOW',
    'the class metaclass likewise';

# A HOW subclass is an ordinary class otherwise.
ok PlainRoleHOW.new ~~ Metamodel::ParametricRoleHOW,
    'a HOW subclass instance still isa its metamodel parent';
