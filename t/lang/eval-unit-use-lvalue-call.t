use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';

# A `use` at the top of an EVAL'd unit fills the unit's own import set, and an
# lvalue call of the imported routine (`substr($s, 4) = 'dy'` through
# P5substr's rw multi) finds it. Checked against rakudo.

plan 3;

is EVAL(Q[use P5substr; my $name = 'fred'; substr($name, 4) = 'dy'; $name]), 'freddy',
    'an imported rw routine called as an lvalue inside an EVAL';

is EVAL(Q[my $o = '1234'; { use P5substr; substr($o, 1, 2) = 'a'; }; $o]), '1a4',
    'and in a block of the EVAL';

use experimental :rakuast;
is EVAL(Q[use P5substr; my $name = 'fred'; substr($name, 4) = 'dy'; $name].AST), 'freddy',
    'and as EVAL($ast)';
