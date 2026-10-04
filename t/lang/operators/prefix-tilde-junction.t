use Test;

plan 8;

# prefix:<~> is not Mu-typed, so a Junction operand autothreads (#11759).
isa-ok ~(1|2), Junction, '~(1|2) is a Junction';
isa-ok ~("a"|"b"), Junction, '~("a"|"b") is a Junction';
my $j = ~(True|False);
isa-ok $j, Junction, 'a stored ~(True|False) is a Junction';
ok ~(1|2) eq "2", 'threaded Strs compare per eigenstate';
ok ~(1&2) eq "1" ?? False !! True, 'all-junction does not match a single eigenstate';
is (~(1|2)).gist, 'any(1, 2)', 'gist lists eigenstates';
ok (~(1|2)) eq "1", 'any matches the first eigenstate';
is ~(3), "3", 'non-junction operand unchanged';
