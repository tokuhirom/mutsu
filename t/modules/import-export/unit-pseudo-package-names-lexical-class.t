use Test;
use lib 't/lib';
use UnitPseudoLexicalClass;

# `UNIT::name` names a lexical class of the compunit, also from a closure that
# runs after the module is loaded.
plan 4;

is lexical-errno.^name, 'errno', 'a Proxy FETCH closure returns the lexical class';
is lexical-errno.Str, 'E!', 'and its methods dispatch';
is own-type().answer, 42, 'from a plain sub of the module';
is own-type().^name, 'errno', 'the type object';
