use Test;

plan 7;

# A three-level store whose middle step is a Pair held by an array element
# (`@a[1]<s><e> = v`, `@a[1] = s => {...}`) writes into the Pair's value.
my @b = 1, (s => {e => "hello"});
@b[1]<s><e> = "world";
is @b[1]<s><e>, 'world', 'array element, Pair, key';

@b[1]<s><f> = "new";
is @b[1]<s>.sort.map(*.key).join(','), 'e,f', 'a new key in the Pair value';

my @a = (x => {e => "abcdef"},);
@a[0]<x><e>.substr-rw(0, 1) = "Q";
is @a[0]<x><e>, 'Qbcdef', 'substr-rw through the chain (PDF ObjStm shape)';

my %h = a => (b => {c => 1});
%h<a><b><c> = 5;
is %h<a><b><c>, 5, 'hash, Pair, key';

my @n = ((k => [1, 2, 3]),);
@n[0]<k>[1] = 9;
is @n[0]<k>.join(','), '1,9,3', 'array element, Pair, positional';

my %ast = ind-obj => [1, 2, (stream => {encoded => "abcdef"})];
%ast<ind-obj>[2]<stream><encoded>.substr-rw(3, 1) = 'x';
is %ast<ind-obj>[2]<stream><encoded>, 'abcxef', 'a four-level chain';

throws-like { my @z = (p => 1,); @z[0]<q><r> = 2 }, X::Assignment::RO,
    'a Pair holding a non-container is still read-only';
