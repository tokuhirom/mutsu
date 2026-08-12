use v6;
use Test;

# Subscript adverbs on QuantHash (Set/Bag/Mix) slices behave as on the
# equivalent key => weight hash, with unreached keys absent.
# (Text::CSV's header separator detection: `$hdr.comb.Bag{$sep-set.list}:kv`)

plan 10;

my $b = "a,b;c,d".comb.Bag;
is-deeply ($b{",", ";"}:kv), (",", 2, ";", 1), 'Bag slice :kv';
is-deeply ($b{",", ";"}:v),  (2, 1),           'Bag slice :v';
is-deeply ($b{",", ";"}:k),  (",", ";"),       'Bag slice :k';
is-deeply ($b{",", "!"}:kv), (",", 2),         'Bag slice :kv drops missing keys';
is-deeply ($b{","}:kv),      (",", 2),         'Bag single-key :kv';

my $s = <x y>.Set;
is-deeply ($s{"x", "z"}:kv), ("x", True), 'Set slice :kv (missing key dropped)';
is-deeply ($s{"x", "z"}:v),  (True,),     'Set slice :v';

my $m = (a => 0.5, b => 1.5).Mix;
is-deeply ($m{"a", "b"}:v).sort, (0.5, 1.5), 'Mix slice :v';

# The header-detection shape itself.
my $hdr = "bAr;foo";
my @sep-set = ",", ";";
my %sep = $hdr.comb.Bag{@sep-set.list}:kv;
is-deeply %sep, {";" => 1}, 'Bag slice :kv into a hash keeps only present separators';
my %conflict = "a,b;c,d".comb.Bag{@sep-set.list}:kv;
is %conflict.elems, 2, 'conflicting separators are both reported';

done-testing;
