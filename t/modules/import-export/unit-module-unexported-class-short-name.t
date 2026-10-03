use Test;
use lib 't/lib';
use UnexportedClassUnit;
use UnexportedClassSecondUser;

plan 6;

throws-like { EVAL 'Hidden.value' }, X::Undeclared::Symbols,
    'a module private class has no short name in the first importer';
is Public.value, 'public', 'an exported class keeps its short name';
is UnexportedClassUnit::Hidden.value, 'hidden',
    'the private class remains available by its qualified name';
is own-hidden(), 'hidden', 'the declaring module sees its own private class';
is second-user-hidden(), 'X::Undeclared::Symbols',
    'an already loaded module does not replay private short names';
is second-user-public(), 'public',
    'an already loaded module replays exported short names';
