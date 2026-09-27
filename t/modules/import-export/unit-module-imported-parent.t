use lib 't/lib';
use Test;

# `grammar G is export` exports the grammar like `class C is export` does, and
# a class declared inside a `unit module` that inherits from a type the module
# imported gets that type as its parent — not a phantom `Module::Name`.
# Found via the Usage::Utils distribution (`grammar UsageStr is BasePaths`,
# with `BasePaths` imported from Parse::Paths).

plan 8;

use ExportedGrammarChild;

ok ChildG ~~ Grammar, 'grammar inheriting an imported grammar is a Grammar';
ok ChildG.parse('123'), 'overridden token is used';
nok ChildG.parse('abc'), 'overridden token rejects what it should';
is ChildG.^parents[0].^name, 'ExportedGrammarBase::BaseG', 'grammar parent is the imported grammar';
is ChildC.new.hi, 'hi', 'class inheriting an imported class gets its methods';
is ChildC.^parents[0].^name, 'ExportedGrammarBase::BaseC', 'class parent is the imported class';

{
    use ExportedGrammarBase;
    is ::('BaseG').^name, 'ExportedGrammarBase::BaseG', 'an exported grammar is bound in the importer';
    ok BaseG.parse('abc'), 'the imported grammar parses';
}
