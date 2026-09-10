use v6;
use Test;

# `$obj<key>:exists` (associative subscript with the :exists adverb) on a user
# object must dispatch the object's EXISTS-KEY method, mirroring how `$obj<key>`
# dispatches AT-KEY. Previously the subscript :exists path only ever tried
# EXISTS-POS, so an object that does Associative (or `handles` EXISTS-KEY to a
# Hash attribute, as zef's config object does) always reported False.
# A positional subscript (`$obj[i]:exists`) must still dispatch EXISTS-POS.

plan 13;

# --- explicit associative methods ---
class Assoc {
    has %.h;
    method AT-KEY($k)     { %!h{$k} }
    method EXISTS-KEY($k) { %!h{$k}:exists }
}
my $a = Assoc.new(h => {a => 1, b => 2});
ok  ($a<a>:exists),   'EXISTS-KEY via subscript: present key';
nok ($a<z>:exists),   'EXISTS-KEY via subscript: absent key';
nok ($a<a>:!exists),  'EXISTS-KEY via subscript: :!exists negation';
ok  $a.EXISTS-KEY('a'), 'direct EXISTS-KEY still works';

# --- `handles` delegation to a Hash attribute (the zef Zef::CLI config shape) ---
my %hash = (a => 1, b => 2, foo => 3);
my $cfg = class :: {
    has %.hash handles <AT-KEY EXISTS-KEY DELETE-KEY keys values>;
}.new(:%hash);
ok  ($cfg<foo>:exists), 'handles-delegated EXISTS-KEY: present key';
nok ($cfg<nope>:exists), 'handles-delegated EXISTS-KEY: absent key';
is  $cfg<foo>, 3,        'handles-delegated AT-KEY still works';

# --- a Positional object must still use EXISTS-POS for `[i]` ---
class Pos {
    has @.items;
    method AT-POS($i)     { @!items[$i] }
    method EXISTS-POS($i) { 0 <= $i < @!items.elems }
    method elems          { @!items.elems }
}
my $p = Pos.new(items => [10, 20, 30]);
ok  ($p[0]:exists),  'EXISTS-POS via positional subscript: present index';
nok ($p[9]:exists),  'EXISTS-POS via positional subscript: absent index';

# --- native hash / array unaffected ---
my %nh = (x => 1);
my @na = 1, 2, 3;
ok  (%nh<x>:exists), 'native hash subscript :exists (present)';
nok (%nh<y>:exists), 'native hash subscript :exists (absent)';
ok  (@na[2]:exists), 'native array subscript :exists (present)';
nok (@na[9]:exists), 'native array subscript :exists (absent)';
