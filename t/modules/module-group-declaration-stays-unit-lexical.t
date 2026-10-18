use Test;

# A module's file-scope `my T $x .= new` and `my ($a, $b) = ...` parse as a
# group declaration, which declares in the module's own scope like a plain
# `my`. Two modules declaring the same names each keep their own: a routine
# of the second module read the first module's `$REGISTRY`. Checked against
# rakudo.
plan 6;

use lib 't/lib';
need GroupDeclLexA;
need GroupDeclLexB;

is GroupDeclLexA::registry().who, 'A', 'the first module reads its own `.=` variable';
is GroupDeclLexB::registry().who, 'B', 'the second module reads its own `.=` variable';
is GroupDeclLexA::pair(), 'a1 a2', 'the first module reads its own list declaration';
is GroupDeclLexB::pair(), 'b1 b2', 'the second module reads its own list declaration';

ok !(try EVAL q[$REGISTRY]).defined, "the modules' variables do not reach the loader";
is GroupDeclLexA::registry().who, 'A', 'the first module still reads its own after the second loaded';
