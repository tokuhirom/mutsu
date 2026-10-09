use v6;
use lib 't/lib';
use Test;

# From List::Operator::DoublePlus: a module that does
# `our &infix:["\c[DOUBLE PLUS]"] is export = &concat` must load (the escaped
# operator name resolves at BEGIN time as the last statement of the unit), and
# a dispatcher captured from an `our proto` and called from the importer must
# rank its candidates by narrowness, not take the first declared one.

plan 4;

use EscapedOpNameFixture;

is-deeply [11, 12] ++ [13], [11, 12, 13], 'Array ++ Array picks the Array candidate';
is-deeply (1, 2) ++ (3,), (1, 2, 3), 'List ++ List picks the positional candidate';
is-deeply [1] ⧺ [2], [1, 2], 'the escaped-name operator is exported';
is ([1] ++ [2]).^name, 'Array', 'result type is Array';
