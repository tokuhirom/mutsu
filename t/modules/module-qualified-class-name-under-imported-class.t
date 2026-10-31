use lib 't/lib';
use Test;

# Found via OO::Plugin: `use A; use B;` where A exports `class Plugin` and B
# declares and exports `class Plugin::Registry` (after importing A).
# `Plugin::Registry` resolved through the imported class `Plugin` and died with
# "Could not find symbol '&Registry' in 'A::Plugin'".
plan 3;

use NestedNameBase;
use NestedNameUser;

is Plugin::Registry.instance, "reg", "the declared class wins over a member lookup in the imported class";
is Plugin.hi, "plugin", "the imported class itself is unchanged";
ok Plugin::Registry.^name.ends-with("Plugin::Registry"), "and names itself";
