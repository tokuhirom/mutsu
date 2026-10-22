use Test;
use lib 't/lib';

# An `EVAL`'s top-level `use` declares what it imports as the EVAL's own
# lexicals, as a block's `use` does (ADR-8751, #14140): the snippet and a
# closure it returns see the module's class and constants, while a later
# `EVAL`, the caller and a routine the snippet calls do not. Checked against
# rakudo.

plan 9;

sub callee { (try ::('UibFoo').hi) // 'no type' }

is EVAL(q[use UseInBlockTypesConsts; UibFoo.hi ~ ' ' ~ UIB-KONST ~ ' ' ~ uib-lower]),
    'imported 42 lower', 'the snippet sees the imported class and constants';
is (try EVAL q[UibFoo.hi]) // 'no type', 'no type', 'a later EVAL does not see the class';
is (try EVAL q[UIB-KONST]) // 'no const', 'no const', 'nor the constant';
is EVAL(q[use UseInBlockTypesConsts; callee()]), 'no type',
    'a routine the snippet calls does not see the class';
my &c = EVAL q[use UseInBlockTypesConsts; -> { UibFoo.hi ~ UIB-KONST }];
is c(), 'imported42', 'a closure the snippet returns keeps them';
is (try EVAL q[UibFoo.hi]) // 'no type', 'no type', 'still gone after the closure ran';

# A hook's literal type key (`'MyAlias' => Int`) in an EVAL and in a block.
is EVAL(q[use UnitModuleExportSub; MyAlias.^name]), 'Int', 'a hook type key in an EVAL';
{
    use UnitModuleExportSub;
    is MyAlias.^name, 'Int', 'and in a block';
}
is (try EVAL q[MyAlias.^name]) // 'gone', 'gone', 'the block\'s ends with it';
