# From the Wheel distribution: `use ::('⊙')` names a module through a constant
# string, and META6.json `provides` maps that non-identifier name to a file whose
# exports (here a term) must be visible to the importer at parse time.
use lib $?FILE.IO.parent.parent.parent.add('fixtures/use-indirect-module-name').Str;
use Test;

plan 3;

{
    use ::('⊙');
    is ⊥, 'bottom', 'term exported through the indirect alias is parsed';
    is shout('a'), 'A', 'routine exported through the indirect alias';
}
{
    use ::("Alias");
    is shout('b'), 'B', 'double-quoted indirect name of an ordinary module';
}
