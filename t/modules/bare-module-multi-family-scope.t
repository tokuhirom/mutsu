use Test;
use lib 't/lib';
use BareMultiScope;

# A package-less module's `multi`/`proto` families are lexical to its
# compunit (#11004): `use` imports the exported ones, an unexported family
# stays private to the module, and the module's own code still reaches it.
# Every expectation was checked against Rakudo.

plan 9;

is bms-proto(1), 'int', 'an exported proto-led family is imported by use';
is bms-proto('a'), 'str', 'with every candidate';
is bms-multi(1), 'm-int', 'an exported multi family is imported by use';
is &bms-multi.candidates.elems, 2, '&name of an imported family sees its candidates';

is bms-call-private(1), 'p-int', "the module's own sub reaches its unexported multi";
is bms-private-block()('x'), 'p-str', "a block from the module reaches its unexported multi";
is BmsBox.new.go(2), 'p-int', "a method of the module's class reaches its unexported multi";

throws-like { EVAL 'bms-private(1)' }, X::Undeclared::Symbols,
    'an unexported multi family does not reach the importer';
nok (try EVAL '&bms-private.candidates.elems'), 'nor does its &name';

