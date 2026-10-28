use Test;
use lib $*PROGRAM.parent(3).add('lib');

plan 2;

# A `where` on a multi method's invocant parameter is compiled in the
# method's declaring compunit, so a type only that unit imported must resolve
# while the candidate is picked (Red's ColumnMethods: `.returns ~~ Json`).
# This file never names `InvocantWhereTag`.
use InvocantWhereCol;

class Col does InvocantWhereCol::Meths { has $.returns }

is Col.new(returns => tag-type()).kind, "tag", 'the unit-imported type matches';
is Col.new(returns => Int).kind, "other", 'the other candidate is picked';
