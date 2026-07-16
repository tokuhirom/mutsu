use v6;
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Fancy/lib").Str;

plan 3;

# A module `use`d with import tags inside one block, then `use`d again in a
# sibling block, must re-import correctly. Popping the first block's import
# scope used to drop the module's OWN fully-qualified source definitions, so
# the second block's re-import had nothing to alias ("Unknown function").
# Root cause of roast/integration/advent2009-day12.t.

{
    use Fancy::Utilities :greet;
    is lolgreet('A'), 'O HAI A', 'first block: :greet exports lolgreet';
}
{
    use Fancy::Utilities :greet, :lolcat;
    is lolgreet('B'), 'O HAI B', 'second block re-imports lolgreet';
    is lolrequest('Cake'), 'I CAN HAZ A CAKE?', 'second block :lolcat export';
}
