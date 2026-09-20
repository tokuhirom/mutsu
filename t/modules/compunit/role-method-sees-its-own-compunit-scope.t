use Test;

# A role's methods are lexically inside the ROLE. Composition rewrites the
# method's owner to the consuming class, so unless the role itself anchors the
# frame, the role compunit's own bare terms — imported `constant`s, enum keys,
# type names — are unreachable from a composed method body and silently
# degrade to a plain string.
#
# Came from Selkie (ecosystem): `Selkie::Widget` is a `unit role` that imports
# `Selkie::Alpha`'s `AlphaMode` enum, and every consuming widget class lives in
# a different compunit. `$!applied-fg-alpha = AlphaOpaque` inside the role then
# died with 'expected AlphaMode but got Str ("AlphaOpaque")'.

plan 6;

use lib $?FILE.IO.parent(3).add('lib').Str;
use RoleLexicalScopeTypes;
use RoleLexicalScopeConsumer;

my $c = RoleLexicalScopeConsumer.new;

is $c.role-constant, 42,
    'an imported `constant` resolves in a composed role method';
is $c.role-sub, 'helped',
    'an imported sub resolves in a composed role method';
is $c.role-type, 'RoleLexicalScopeTypes::Handle',
    'an imported type name resolves in a composed role method';
is $c.role-enum-bare, Sour,
    'an imported enum key resolves by its bare spelling in a composed role method';

# The attribute is typed by the imported enum, so a bareword that came back as
# a Str would fail the type check rather than merely comparing unequal.
is $c.flavour, Sweet, 'the role attribute default is the enum value, not its name';
is $c.set-flavour-bare, Sour,
    'assigning a bare enum key to a typed role attribute type-checks';
