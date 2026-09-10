use v6;
use Test;

plan 7;

# An itemized Pair (`$(:a(1))`) or a Pair held through an element cell (e.g. a
# classify bucket element) still counts as a hash initializer pair — raku
# accepts both. An itemized *hash* element still dies X::Hash::Store::OddNumber.
# Surfaced by zef: Ecosystems.search does
# `@raku-specs.grep(*.defined).hash` over classify bucket elements.

my %m1 = ($(:a(1)),);
is %m1<a>, 1, 'itemized Pair accepted by hash list-assignment';

is ($(:a(1)),).hash<a>, 1, 'itemized Pair accepted by .hash';

my %h = a => 1;
my %bucket-hash = %h.classify({ "X" })<X>.grep(*.defined).hash;
is %bucket-hash<a>, 1, 'classify bucket element (cell-held Pair) accepted by .hash';

my %src = a => 1, b => 2;
my %roundtrip = %src.classify({ "all" })<all>.hash;
is %roundtrip.elems, 2, 'classify bucket of a whole hash rebuilds the hash';
is %roundtrip<b>, 2, 'rebuilt hash keeps values';

# itemized hash element still dies like raku
my %h2 = x => 1;
throws-like { my %m = ($%h2,); }, Exception,
    message => /'Odd number of elements'/,
    'itemized hash element still dies Odd number';

# even count of non-pair elements still key/value-pairs up
my %kv = ("k", "v").hash;
is %kv<k>, 'v', 'flat key/value list still works';

done-testing;
