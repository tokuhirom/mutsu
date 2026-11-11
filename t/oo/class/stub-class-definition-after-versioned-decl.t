use lib 't/lib';
use Test;

# A module that stubs a type, then declares a versioned (`:ver<...>`) type, then
# gives the stubbed type its real definition with the versioned one as parent.
# mutsu moves a stubbed class's real definition up next to its stub unless a
# type declaration intervenes; a versioned one did not count, because it
# arrives wrapped together with its `:ver`/`:auth` metadata call, so the
# child was registered before its parent (CSS::Grammar: "'CSS::Grammar::Core'
# cannot inherit from 'CSS::Grammar' because it is unknown").

plan 5;

use StubThenVersionedParent;

ok StubThenVersionedParent::Core.parse('hello'), 'the grammar child parses through its versioned parent';
is StubThenVersionedParent.^ver, v0.5.0, 'the versioned grammar keeps its version';
is StubThenVersionedParent::Core.^mro[1].^name, 'StubThenVersionedParent', 'the child inherits from it';
is StubThenVersionedClass::Kid.hi, 'parent', 'a stubbed class inherits from a versioned class declared between';
is StubThenVersionedClass.^ver, v1.2, '... which keeps its version';
