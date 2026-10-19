use Test;
use lib $?FILE.IO.parent(2).add('lib');

# A module's `my package P` is lexical to the module. A script that names
# `$P::x` reaches a separate package autovivified under its own GLOBAL, so
# the module's `$P::x` and the script's are two variables (#11156).

plan 5;

use MyPackageLexicalState;

nok get-tz().defined, "the module's variable starts undefined";
$MyPackageLexicalState::tz = 'main';
nok get-tz().defined, "the script's assignment does not reach the module's variable";
put-tz('p');
is get-tz(), 'p', "the module's own subs write and read its variable";
is $MyPackageLexicalState::tz, 'main', "the script's variable keeps its own value";
is $GLOBAL::MyPackageLexicalState::tz, 'main', '... which is the GLOBAL one';

# vim: expandtab shiftwidth=4
