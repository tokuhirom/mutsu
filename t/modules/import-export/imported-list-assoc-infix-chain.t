use Test;

# FunctionalParsers t/12: an imported plain `sub infix:<op>` declared
# `is assoc<list>` must take a whole `1 op 2 op 3` chain; it died with
# "Two terms in a row" because only the two-operand infix site consulted the
# block's lexical import set.
plan 6;

use lib 't/lib';
use ListAssocInfixChain;

is (1 lsx 2), "LS(1,2)", 'two operands';
is (1 lsx 2 lsx 3), "LS(1,2,3)", 'a slurpy list-assoc chain gets every operand';
is ((1 lsx 2) lsx 3), "LS(LS(1,2),3)", 'parentheses still group';
is (1 l3x 2 l3x 3), "L3(1,2,3)", 'a fixed-arity list-assoc sub gets three operands';
is (1 l3x 2), "L3(1,2,-)", 'its optional third operand stays optional';
is (1 lmx 2 lmx 3), "LM(1,2,3)", 'a multi family chain is unchanged';
