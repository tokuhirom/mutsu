use Test;

# `S///` on a Junction topic autothreads over the eigenstates and yields a
# Junction, as `.subst` does (#15627, from Data::Dump::Tree's `S:g/.../ given`).

plan 6;

my $v = 'axb' | 'cxd';
my $r = S/x// given $v;
isa-ok $r, Junction, 'S/// on a Junction topic is a Junction';
ok $r.chars.all == 2, 'each eigenstate was substituted';
ok ($r eq 'ab') && ($r eq 'cd'), 'the substituted texts are the eigenstates';

my $g = S:g/x// given 'axxb' | 'xcx';
isa-ok $g, Junction, 'S:g/// on a Junction topic is a Junction';
ok $g.chars.any == 2 && $g.chars.any == 1, 'every occurrence in each eigenstate was removed';

is (S:g/x// given 'axb').chars, 2, 'a plain string topic is unchanged';
