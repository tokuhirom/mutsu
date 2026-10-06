use Test;

# A `unit package P;` inside an EVAL string scopes only that EVAL: the next
# EVAL starts in the caller's package again, so its types are not declared as
# `P::K` (#12135). Every expected answer is Rakudo 2026.09's.

plan 5;

is EVAL('unit package P12;').raku, 'P12', 'a unit package answers its type object';
is EVAL('class K1 { }').raku, 'K1', 'a later EVAL does not inherit the unit package';
is EVAL('my package Q { 1 }; Q').raku, 'Q', 'a later `my package` is not nested in it';
EVAL('unit module M12;');
is EVAL('class K2 { }').raku, 'K2', 'a unit module does not leak either';
is EVAL('$?PACKAGE.^name'), 'GLOBAL', 'the next EVAL is in GLOBAL';
