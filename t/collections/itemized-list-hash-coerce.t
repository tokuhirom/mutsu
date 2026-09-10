use Test;

plan 6;

# `.hash` / `%(...)` on an itemized list of pairs must flatten the pairs into a
# Hash, not treat the itemized list as a single opaque element (which wrongly
# raised X::Hash::Store::OddNumber). This blocked DBIish, whose connect path
# does `|%($*DBI-DEFS<ConnDefaults>)` where ConnDefaults is an itemized list of
# colon-pairs.

my $cd = $(:RaiseError, :!PrintError, :AutoCommit);

is $cd.WHAT.^name, 'List', 'source is an itemized List';

my %h = $cd.hash;
is %h<RaiseError>, True,  '.hash flattens :RaiseError';
is %h<PrintError>, False, '.hash flattens :!PrintError';
is %h<AutoCommit>, True,  '.hash flattens :AutoCommit';

# %(...) coercion form
my %g = %($cd);
is %g.elems, 3, '%($itemized-list) builds a 3-key hash';

# A plain (non-itemized) list of pairs still works
my %f = (:a, :b).hash;
is %f.elems, 2, '.hash on a flat pair list still works';
