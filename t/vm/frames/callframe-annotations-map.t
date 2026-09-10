use v6;
use Test;

plan 5;

# CallFrame.annotations is an immutable Map, not a mutable Hash.
my $ann = callframe.annotations;
is $ann.^name, 'Map', 'annotations returns a Map';
ok $ann ~~ Map, 'annotations does Map';
nok $ann ~~ Hash, 'annotations is not a (mutable) Hash';

# The <file>/<line> lookups still resolve.
is $ann<file>, callframe.file, 'annotations<file> matches .file';
ok $ann<line>.defined, 'annotations<line> is defined';
