use Test;

# #14539: `Str.ACCEPTS(CompUnit)` compares the compunit's short-name
# (CompUnit::Util's `find-loaded('Test')` smartmatches a name against `.loaded`).

plan 3;

my @loaded = $*REPO.repo-chain.map(*.loaded).flat;
my $test = @loaded.first(*.short-name eq 'Test');
isa-ok $test, CompUnit, 'the Test compunit is loaded';
ok 'Test'.ACCEPTS($test), 'Str.ACCEPTS(CompUnit)';
ok $test ~~ 'Test', 'CompUnit ~~ Str';
