use Test;

plan 6;

# Found by PrettyDump 1.2.3: an is-Hash subclass keeps positional
# key/value constructor arguments in its Associative backing store.
class EntryHash is Hash { }

my $h = EntryHash.new: 'abc', '123', 'xyz', 'def';
is $h.^name, 'EntryHash', 'positional construction keeps the subclass type';
is $h.elems, 2, 'positional constructor arguments create two entries';
is $h<abc>, '123', 'the first positional key/value pair is stored';
is $h<xyz>, 'def', 'the second positional key/value pair is stored';

my $from-array = EntryHash.new: ['left', 1, 'right', 2];
is $from-array<left>, 1, 'an array argument is flattened into hash entries';
is $from-array<right>, 2, 'flattened array arguments preserve later pairs';
