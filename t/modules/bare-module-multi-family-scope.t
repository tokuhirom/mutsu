use Test;
use lib 't/lib';
use BareMultiScope;
use BareMultiScopeUser;

# A package-less module's `multi`/`proto` families are lexical to its
# compunit (#11004): `use` imports the exported ones, an unexported family
# stays private to the module, and the module's own code still reaches it.
# Every expectation was checked against Rakudo.

plan 12;

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


# A module that never imported the family still dispatches a `&name` its
# caller hands it: the code value carries the family, whatever unit calls it.
is bmsu-apply(&bms-digest, blob8.new(1, 2)), 'digest:2:7',
    'a passed-in &name of a scoped family dispatches by type in another module';
is bmsu-apply(&bms-digest, 'abc'), 'digest:3:7', 'including through samewith';
is bmsu-apply-in-block(&bms-multi, 'q'), 'm-str', 'and from a block in that module';
