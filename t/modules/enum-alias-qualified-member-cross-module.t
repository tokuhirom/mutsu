use Test;

# #14174: `Mod::Alias::Member` where `our constant Alias = Other::Enum` is
# declared in a loaded module (from KeyNub::LicDongle).
use lib 't/lib';
use EnumAliasScope;

plan 3;

is EnumAliasScope::Scope::Device.value, 0, 'member through the imported constant alias';
is EnumAliasScope::Scope::Developer.value, 1, 'second member';
is EnumAliasScope::Scope::Device.WHAT.^name, 'Scope', 'it is the enum value';
