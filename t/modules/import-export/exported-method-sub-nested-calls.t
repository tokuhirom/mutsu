use v6;
use lib 't/lib';
use Test;

# CSS::Selector::To::XPath (#15094): a method exported with `is export` and
# called through its sub form ran its body with every local read as a Scalar
# container, so `.from` on a stored grammar Match found no such method.

use ExportedMethodNested :parse;

plan 3;

is ExportedMethodNested.parse-from(:text<x>), 0, 'called as a method';
is parse-from(:text<x>), 0, 'called through its exported sub form';
is parse-from(:text<x>), 0, 'and again';
